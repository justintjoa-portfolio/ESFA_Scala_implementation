package Circuit.Components.Primitive.CircuitComponent.State

import Circuit.Components.Primitive.MemoryCell.MemoryCell

case class CircuitComponentState(
                                  memoryCell:MemoryCell,

                                  inputPort: Int,

                                  inputReference: Int,

                                  outputPort: Int,

                                  outputReference: Int
                                )


