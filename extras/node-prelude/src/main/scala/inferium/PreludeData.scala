package inferium

import ujson.Js

case class PreludeData(global: Js.Obj, modules: Js.Arr, types: Js.Arr)
