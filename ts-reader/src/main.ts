import * as ts from "typescript";
import * as fs from 'fs'
import { join, basename, dirname } from 'path'
import * as p from 'path'
import { shell } from 'execa'
import * as os from 'os'
import assert from 'assert'

/*function compile(fileNames: string[], options: ts.CompilerOptions): void {
    let program = ts.createProgram(fileNames, options);
    const tc = program.getTypeChecker()
    program.getSourceFiles().forEach(file => {
        console.log(file.fileName, file.isDeclarationFile)
        file.referencedFiles.forEach(ref => console.log("  ", ref.fileName))
    })

    tc.getAmbientModules().forEach(sym => {
        const mem = sym.exports
        console.log(sym.name)
        if (mem && sym.getName() == "\"fs\"") {
            mem.forEach(sym => {
                console.log("=>", sym.name)
                if (sym.name == "write") {
                    sym.declarations!.forEach( decl => {
                        console.log("  =>", decl)
                    })
                }
            })
        }
    })
    
    console.log("========")
    
    let file = program.getSourceFiles()[3]
    let l = file.getChildAt(0)
    //scrollBy()
    let syms = tc.getSymbolsInScope(l, ts.SymbolFlags.BlockScopedVariable)
    syms.forEach(sym => console.log(sym.name))
    
    console.log("had h:", syms.some(sym => sym.name == "h"))
    console.log("had test:", syms.some(sym => sym.name == "test"))
    console.log("had libfunc:", syms.some(sym => sym.name == "libfunc"))
    
    //program.getTypeChecker().getAmbientModules
    //program.getSourceFiles()[2].getChildren().forEach(dir => console.log("  =>", ts.SyntaxKind[dir.kind]))
}

compile(process.argv.slice(2), {
    noEmitOnError: true, noImplicitAny: true,
    target: ts.ScriptTarget.ES5, module: ts.ModuleKind.CommonJS
});}*/


const tmpdir = "./tmp"

function findPackageRoot(pkgname: string, path: string): string {
    function found(): Boolean {
        return basename(path) == pkgname
            && basename(dirname(path)) == "node_modules"
            && fs.existsSync(join(path, "package.json"))
    }

    while (!found()) {
        path = dirname(path)
    }

    return join(path)
}

const persistant = true

const dirsurfix = persistant? "" : "-" + Date.now()
const orgcwd = process.cwd()
const dir = join(os.tmpdir(), "inferium")
process.chdir(dir)


function here(path: string) {
    assert(p.isAbsolute(path))
    //path = p.isAbsolute(path) path : p.normalize(join(process.cwd(), path))
    const nm_dir = join(dir, "node_modules")
    if (!p.relative(nm_dir, path).startsWith("..")) {
        return "<inf>/" + p.relative(nm_dir, path)
    } else if (!p.relative(orgcwd, path).startsWith("..")) {
        return "<org>/" + p.relative(orgcwd, path)
    } else {
        return path
    }
}

function getDependencies(path: string, found: Set<string>) {
    console.debug("Look into", here(path))
    if (found.has(path)) {
        return
    }
    found.add(path)

    let pkgjson = require(join(path, "package.json"))

    const deps = pkgjson.dependencies || {}

    Object.keys(deps)
        .filter((key) => !/^@types\//.test(key))
        .forEach(dep => {
            const deppath = require.resolve(dep, {paths: [path]})
            getDependencies(findPackageRoot(dep, deppath), found)
        })
}


async function main(pkgname: string) {

    const skipInstall = fs.existsSync(dir)

    async function installType(path: string) {
        const pkg = basename(path)
        console.log("Install types for", pkg)
        const res = await shell(`npm install '@types/${pkg}' --prefix '${dir}'`)
        if (res.failed) {
            console.log("=> not found")
        }
    }

    if (!skipInstall) {
        console.log(`Install ${pkgname} into ${dir}`)
        let res = await shell(`npm install '${pkgname}' --prefix '${dir}'`)
        if (res.failed) {
            console.log("Failed! Error:", res.stderr)
            return
        }
        console.log(`Done.\n`)
    } else {
        console.log("Skip downloading...")
    }

    console.log("Search types...")
    const mainpkg = join(dir, "node_modules", pkgname)
    const deps = new Set<string>()
    getDependencies(mainpkg, deps)
    deps.delete(mainpkg)

    if (!skipInstall) {
        for (const dep of deps) {
            await installType(dep)
        }

        console.log("Force installation of node types...")
        installType("/node")
    }
    console.log("Done.\n")


    console.log("Gather types...")
    const imports = Array.from(deps).map((p) => basename(p))

    //const emptyTsPath = join(mainpkg, "_inferium_empty.ts")
    const gatherTsPath = join(mainpkg, "_inferium_gather.ts")
    //console.log("Empty: ", here(emptyTsPath))
    console.log("File:", here(gatherTsPath))

    console.log(`Gather types for ${imports.length} packages:`)
    console.log("=>", imports.join(", "), "\n")

    function mkn(i: number) {
        return `_inferium_import_${i}`
    }

    const importNames = imports.map((_, i) => mkn(i))

    {
        //fs.writeFileSync(emptyTsPath, "")

        console.log("Write gather file")

        const importStmts = imports.map((m, i) => `import * as ${mkn(i)} from '${m}'`)
        const logStmts = imports.map((_, i) => `console.log(i${mkn(i)})`)
        fs.writeFileSync(gatherTsPath, importStmts.join("\n") + "\n" + logStmts.join("\n"))
        console.log("Done.\n")
    }


    console.log("Analyse program")
    const program = ts.createProgram([gatherTsPath], { rootDir: dir, target: ts.ScriptTarget.ES5, module: ts.ModuleKind.CommonJS });
    const tc = program.getTypeChecker()

    let gatherFile = program.getSourceFile(gatherTsPath)!
    //scrollBy()
    let symbols = tc.getSymbolsInScope(gatherFile, ts.SymbolFlags.ModuleMember)
    //syms.forEach(sym => console.log("=>", sym.name))
    
    const importedSymbols = symbols.filter(({name}) => importNames.indexOf(name) >= 0)
    const globalSymbols = symbols.find(({name}) => name == "global")!

    console.log("Done.\n")

    console.log("global:", globalSymbols.name)
    console.log("imported:", importedSymbols.map(({name})=>name).join(", "))
    console.log("ambient modules:", tc.getAmbientModules().map(m => m.name).join(", "))

    console.log("\nIncorporated files:")

    program.getSourceFiles().forEach(file => {
        console.log("=>", here(file.fileName))
    })

    let x = tc.getAmbientModules()[0]
    let y = x.declarations
}

main(process.argv[2]).catch(err => {
    console.error("Error:", err)
})

export namespace ns {
	export function test() {
		
	}
}