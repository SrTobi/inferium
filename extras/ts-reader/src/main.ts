import * as ts from "typescript";
import * as fs from 'fs'
import { join, basename, dirname } from 'path'
import * as p from 'path'
import { shell } from 'execa'
import * as os from 'os'
import assert from 'assert'
import c from 'chalk'

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
const dir = join(os.tmpdir(), "inferium" + dirsurfix)


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

    process.chdir(dir)

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
        await installType("/node")
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
    const program = ts.createProgram([gatherTsPath], {
        rootDir: dir,
        target: ts.ScriptTarget.ES5, module: ts.ModuleKind.CommonJS,
        strict: true
    });
    const tc = program.getTypeChecker()

    let gatherFile = program.getSourceFile(gatherTsPath)!
    //scrollBy()
    let symbols = tc.getSymbolsInScope(gatherFile, ts.SymbolFlags.ModuleMember)
    //syms.forEach(sym => console.log("=>", sym.name))
    
    const importedSymbols = symbols.filter(({name}) => importNames.indexOf(name) >= 0)
    const globalSymbol = symbols.find(({name}) => name == "global")

    console.log("Done.\n")

    if (globalSymbol) {
        console.log("global:", globalSymbol.name)
    } else {
        console.log("Couldn't find global symbol")
        return;
    }
    console.log("imported:", importedSymbols.map(({name})=>name).join(", "))
    console.log("ambient modules:", tc.getAmbientModules().map(m => m.name).join(", "))

    console.log("\nIncorporated files:")

    program.getSourceFiles().forEach(file => {
        console.log("=>", here(file.fileName))
    })

    console.log("")
    
    let predef = createPredefTypes(program, gatherFile, globalSymbol)
    
    console.log("Done.\n")

    const resultPath = join(orgcwd, "result.json")
    console.log("Write result to", here(resultPath))
    fs.writeFileSync(resultPath, JSON.stringify(predef))
    console.log("Done.")
}

interface PredefTypes {
    globalType: TypeInfo
    ambientModules: Module[]
    types: Type[]
}

interface Module {
    name: string
    type: TypeInfo
}

interface Type {
    id: number
    name?: string
    qualifiedName?: string
    declarationFiles?: string[]
    isAlias: boolean
    aliasTypeParameter?: Generic[]
    type: TypeInfo
}

type TypeInfo = number | TypeInfoBase

interface TypeInfoBase {
    type: string
}

interface Primitive extends TypeInfoBase {
    value?: string | boolean | number
}

interface Union extends TypeInfoBase {
    type: "union"
    types: TypeInfo[]
}

interface Intersection extends TypeInfoBase {
    type: "intersection"
    types: TypeInfo[]
}

interface Index extends TypeInfoBase {
    type: "index"
    keyof: TypeInfo
}

interface IndexAccess extends TypeInfoBase {
    type: "index-access"
    object: TypeInfo
    index: TypeInfo
    constraint: TypeInfo | undefined
}

interface Interface extends TypeInfoBase {
    type: "interface"
    isClass: boolean
    bases: TypeInfo[]
    callSignatures: CallSignature[]
    constructionSignatures: CallSignature[]
    properties: {
        [name: string]: Property
    }
}

interface Tuple extends TypeInfoBase {
    type: "tuple"
    member: TypeInfo[]
}

interface Property {
    type: TypeInfo
    optional: boolean
    readonly: boolean
}

interface CallSignature {
    typeParameter: Generic[]
    returnType: TypeInfo
    parameters: Parameter[]
}

interface Parameter {
    name: string
    type: TypeInfo
    optional: boolean
    //default: boolean
}

interface Generic {
    id: number
    constraint?: TypeInfo
}

interface GenericsReference {
    type: "generics-ref"
    targetId: number
}

interface Reference extends TypeInfoBase {
    type: "ref"
    targetId: number
    typeParameter: TypeInfo[]
}


function createPredefTypes(program: ts.Program, gatherFile: ts.Node, globalSymbol: ts.Symbol): PredefTypes {
        
    function p(type: string, value?: string | number | boolean): Primitive {
        const prim: Primitive = { type, value }
        return prim
    }
    
    const tc = program.getTypeChecker()

    function resolveSymbol(sym: ts.Symbol): TypeInfo {
        const type = tc.getTypeOfSymbolAtLocation(sym, gatherFile)
        // these functions take html Elements which adds 3mb html types which are never needed!
        if (sym.name == "msIsIndependentlyComposed" || sym.name == "select") {
            return p("any")
        }
        return resolveType(type)
    }

    function idof(type: ts.Type): number {
        return (<any>type).id
    }

    function ref(id: number, typeParameter?: TypeInfo[]): number | Reference {
        if (typeParameter && typeParameter.length > 0) {
            return {
                type: "ref",
                targetId: id,
                typeParameter
            }
        } else {
            return id
        }
    }

    const types: Type[] = []
    const foundTypes = new Set<number>()

    function resolveType(type: ts.Type): TypeInfo {
        const id = idof(type)

        if (type.aliasSymbol) {
            const sym = type.aliasSymbol
            const typeParameter = (type.aliasTypeArguments || []).map(ty => resolveType(ty))

            if (!foundTypes.has(id)) {
                /*if (sym.name == "Readonly") {
                    debugger
                }*/
                foundTypes.add(id)
                const alias: Type = {
                    id: idof(type),
                    name: sym.name,
                    qualifiedName: tc.getFullyQualifiedName(sym),
                    declarationFiles: (sym.declarations || []).map(d => d.getSourceFile().fileName),
                    isAlias: true,
                    aliasTypeParameter: [],
                    type: resolve()
                }
                types.push(alias)
            }

            ref(id, typeParameter)
        }

        return resolve()

        function resolve(): TypeInfo {
            const f = type.getFlags()
            const tf = ts.TypeFlags
            assert(!type.pattern)

            if (f == tf.Any) {
                return p("any")
            } else if (f & tf.BooleanLike) {
                return p("boolean")
            } else if (f == tf.Number) {
                return p("number")
            } else if (type.isNumberLiteral()) {
                return p("number-lit", type.value)
            } else if (f == tf.String) {
                return p("string")
            } else if (type.isStringLiteral()) {
                return p("string-lit", type.value)
            } else if (f == tf.Undefined) {
                return p("undefined")
            } else if (f == tf.Null) {
                return p("null")
            } else if (f == tf.Never) {
                return p("never")
            } else if (f == tf.Void) {
                return p("void")
            } else if (f & tf.ESSymbolLike) {
                return p("symbol")
            } else if (f == tf.NonPrimitive) {
                return p("object")
            } else if (f == tf.TypeParameter) {
                const genref: GenericsReference = {
                    type: "generics-ref",
                    targetId: idof(type)
                }
                return genref
            } else if (f == tf.Object) {
                //console.log("=> Ignored")
                const obj = <ts.ObjectType>type
                const ofs = obj.objectFlags
                if (ofs & ts.ObjectFlags.Reference) {
                    const refty = <ts.TypeReference>obj
                    const target = refty.target
                    const typeArgs = (refty.typeArguments || []).map(ty => resolveType(ty))

                    if (target.objectFlags & ts.ObjectFlags.Tuple) {
                        const tuple: Tuple = {
                            type: "tuple",
                            member: typeArgs
                        }
                        return tuple
                    }

                    const sym = type.getSymbol()
                    assert(sym)
                    ensureObject(target)
                    return ref(id, typeArgs)
                }

                const sym = type.getSymbol()

                if (sym) {
                    ensureObject(obj)
                    return ref(id)
                } else {
                    return makeObject(<ts.InterfaceTypeWithDeclaredMembers>obj)
                }            
            } else {
                if (foundTypes.has(id)) {
                    return id
                }
                foundTypes.add(id)
                const sym = type.getSymbol()
                const decls = sym? sym.getDeclarations() || [] : []

                types.push({
                    id,
                    declarationFiles: decls.map(d => d.getSourceFile().fileName),
                    isAlias: false,
                    type: makeBigTypes()
                })

                return ref(id)
            }

            function makeBigTypes(): TypeInfo {
                if (type.isUnion()) {
                    const union: Union = {
                        type: "union",
                        types: type.types.map(ty => resolveType(ty))
                    }
                    return union
                } else if (type.isIntersection()) {
                    const intersection: Intersection = {
                        type: "intersection",
                        types: type.types.map(ty => resolveType(ty))
                    }
                    return intersection
                } else if (f == tf.Index) {
                    const index: Index = {
                        type: "index",
                        keyof: resolveType((<ts.IndexType>type).type)
                    }
                    return index
                } else if (f == tf.IndexedAccess) {
                    const ty = type as ts.IndexedAccessType
                    const indexAccess: IndexAccess = {
                        type: "index-access",
                        object: resolveType(ty.objectType),
                        index: resolveType(ty.indexType),
                        constraint: ty.constraint? resolveType(ty.constraint) : undefined
                    }
                    return indexAccess
                } else if (f == tf.Conditional) {
                    throw new Error("Conditionals are not supported")
                } else if (f == tf.Enum) {
                    throw 0
                    //assert(false)
                    //return t({ type: "enum" })
                } else {
                    throw new Error("Unexpected Flags")
                }
            }
        }
    }

    function ensureObject(obj: ts.ObjectType): void {
        const sym = obj.getSymbol()
        const id = idof(obj)
        assert(sym)
        if (sym && !foundTypes.has(id)) {
            foundTypes.add(id)
            const namedty: Type = {
                id,
                name: sym.name,
                qualifiedName: tc.getFullyQualifiedName(sym),
                declarationFiles:
                    (sym.getDeclarations() || [])
                        .map(d => d.getSourceFile().fileName)
                        .filter(function(item, i, ar){ return ar.indexOf(item) === i; }),
                isAlias: false,
                type: makeObject(<ts.InterfaceTypeWithDeclaredMembers>obj)
            }
            types.push(namedty)
        }
    }

    function makeObject(obj: ts.InterfaceTypeWithDeclaredMembers): Interface {
        const objsym = obj.getSymbol()
        const pmap: { [name: string]: Property } = {}
        /*if (!obj.declaredProperties) {
            //debugger
        }*/

        let bases = (obj.getBaseTypes() || []).map((ty: ts.Type) => resolveType(ty))
        
        if (objsym && objsym.name.indexOf("HTML") >= 0) {
            debugger
        }

        let properties = obj.getProperties()
        properties = obj.declaredProperties || properties
        properties.forEach(s => {
            const isPrototype = !!(s.getFlags() & ts.SymbolFlags.Property)
            const isReadonly = isPrototype || !!(ts.getCombinedModifierFlags(s.declarations![0]) & ts.ModifierFlags.Readonly)

            pmap[s.name] = {
                type: resolveSymbol(s),
                optional: !!(s.getFlags() & ts.SymbolFlags.Optional),
                readonly: isReadonly
            }
        })

        function resolveCallSignature(sig: ts.Signature): CallSignature {
            const parameters = sig.getParameters().map<Parameter>(param => {
                return {
                    name: param.name,
                    type: resolveSymbol(param),
                    optional: !!(param.getFlags() & ts.SymbolFlags.Optional)
                }
            })

            const typeParameter = (sig.getTypeParameters() || []).map<Generic>(param => {
                const c = param.getConstraint()
                return {
                    id: (<any>param).id,
                    constraint: c? resolveType(c) : undefined,
                }
            })

            const returnType = resolveType(sig.getReturnType())
            
            return {
                typeParameter,
                returnType,
                parameters
            }
        }

        let callsigs = obj.getCallSignatures()
        callsigs = obj.declaredCallSignatures || callsigs
        const callSignatures = callsigs.map(resolveCallSignature)


        let constsigs = obj.getConstructSignatures()
        constsigs = obj.declaredConstructSignatures || constsigs
        const constructionSignatures = constsigs.map(resolveCallSignature)
        
        const intf: Interface = {
            type: "interface",
            isClass: obj.isClass(),
            bases,
            callSignatures,
            constructionSignatures,
            properties: pmap
        }
        return intf
    }

    const globalType = resolveSymbol(globalSymbol)
    
    const ambientModules = tc.getAmbientModules().map(m => ({
        name: m.name,
        type: resolveSymbol(m)
    }))

    return {
        globalType,
        ambientModules,
        types
    }
}

main(process.argv[2])/*.catch(err => {
    console.error("Error:", err)
})*/

export namespace ns {
	export function test() {
		
	}
}