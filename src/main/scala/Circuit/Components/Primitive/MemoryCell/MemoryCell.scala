package Circuit.Components.Primitive.MemoryCell

case class MemoryCell(var array_handle: Int,
                      var array_code: Int, var index: Int,
                      var value: Int,
                      var rank: Int,
                      //rank of the element is the distance (in number of updates) of the element from empty.
                      var reference: Int) {
  //note: the handle of the tail of a potential array in memory,
  //assuming potential array is made array in code
  //is the same as handle of the array




  var arrDef: Boolean = false;


  var eltDef: Boolean = false;

  var low: Int = 0
  var high: Int = 0

  var _notify: Boolean = false;
  var zombie: Boolean = false;

  var select: Boolean = false;

  var mark: Boolean = false;


}
