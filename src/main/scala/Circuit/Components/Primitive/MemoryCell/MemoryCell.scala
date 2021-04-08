package Circuit.Components.Primitive.MemoryCell

case class MemoryCell(var array_handle: Int,
                      var array_code: Int, var index: Int,
                      var value: Int, var rank: Int,
                      var reference: Int) {


  var arrDef: Boolean = false;


  var eltDef: Boolean = false;

  var low: Int = 0
  var high: Int = 0

  var _notify: Boolean = false;
  var zombie: Boolean = false;

  var select: Boolean = false;

  var mark: Boolean = false;


}
