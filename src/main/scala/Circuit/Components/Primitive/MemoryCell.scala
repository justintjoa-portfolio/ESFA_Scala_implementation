package Circuit.Components.Primitive

case class MemoryCell(var array_handle: Int,
                      var array_code: Int, var index: Int,
                      var value: Int, var rank: Int,
                      var low: Int,
                      var high: Int) {


  var arrDef: Boolean = false;


  var eltDef: Boolean = false;


  //notify, zombie, select, and mark will be implemented in the lowest level API
}
