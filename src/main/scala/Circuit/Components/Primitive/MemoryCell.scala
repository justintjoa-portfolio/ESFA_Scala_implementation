package Circuit.Components.Primitive

case class MemoryCell(var array_handle: Int,
                      var array_code: Int, var index: Int,
                      var value: Int, var rank: Int,
                      var low: Int,
                      var high: Int,
                      var reference:Int) {


  var arrDef: Boolean = false;


  var eltDef: Boolean = false;

  var _notify: Boolean = false;
  var zombie: Boolean = false;

  var select: Boolean = false;

  var mark: Boolean = false;




}
