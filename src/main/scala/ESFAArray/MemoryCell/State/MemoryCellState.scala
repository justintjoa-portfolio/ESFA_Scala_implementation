package ESFAArray.MemoryCell.State

case class MemoryCellState(
                            var arrDef: Boolean,
                            var handle: Int,
                            var array_code: Int,
                            var eltDef: Boolean,
                            var rank: Int,
                            var low: Int,
                            var high: Int,
                            var index: Int,
                            var value: Int,
                            var notify_flag: Boolean,
                            var zombie: Boolean,
                            var select: Boolean,
                            var mark: Boolean)
