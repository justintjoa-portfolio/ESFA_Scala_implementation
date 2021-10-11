import ESFAArray.Array.Op.ESFAArrayOp
import ESFAArray.Array.State.ESFAArrayState
import org.scalatest.FunSuite

class Test extends FunSuite {

  val emptyArrayState: ESFAArrayState = ESFAArrayState()

  test("UpdateBasicTest") {
    assert(emptyArrayState.memoryCellStack(0).state.rank === 0)
    var state_and_handle = ESFAArrayOp().update(emptyArrayState, None, 0, 5)

    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 0)
    state_and_handle._2 match {
      case Right(new_array_handle) => {
        assert(new_array_handle === 0)
      }
      case Left(error_message) =>
        print(error_message)
        fail("Should be updating and adding a legitimate entry with handle")
    }

    state_and_handle = ESFAArrayOp().update(state_and_handle._1, Some(0), 2, 10)

    // Assert old array is intact, with updated low and high
    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 1)

    // Assert new array has right behavior
    assert(state_and_handle._1.memoryCellStack(1).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(1).state.index === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(1).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.low === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.high === 1)

    state_and_handle._2 match {
      case Right(new_array_handle) => {
        assert(new_array_handle === 1)
      }
      case Left(error_message) =>
        print(error_message)
        fail("Should be updating and adding a legitimate entry with handle")
    }

    state_and_handle = ESFAArrayOp().update(state_and_handle._1, None, 4, 10)

    // Assert old arrays are intact
    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.handle === 0)
    assert(state_and_handle._1.memoryCellStack(1).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(1).state.index === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(1).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.low === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.high === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.handle === 1)

    // Assert new array has right behavior
    assert(state_and_handle._1.memoryCellStack(2).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(2).state.index === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(2).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(2).state.handle === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.low === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.high === 2)

    state_and_handle._2 match {
      case Right(new_array_handle) => {
        assert(new_array_handle === 2)
      }
      case Left(error_message) =>
        print(error_message)
        fail("Should be updating and adding a legitimate entry with handle")
    }

    state_and_handle = ESFAArrayOp().update(state_and_handle._1, Some(2), 10, 21)

    // Assert old arrays are intact
    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.handle === 0)
    assert(state_and_handle._1.memoryCellStack(1).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(1).state.index === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(1).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.low === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.high === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.handle === 1)

    // Assert new array has right behavior, with updated low/high for the parent
    assert(state_and_handle._1.memoryCellStack(3).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(3).state.index === 10)
    assert(state_and_handle._1.memoryCellStack(3).state.value === 21)
    assert(state_and_handle._1.memoryCellStack(3).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(3).state.handle === 3)
    assert(state_and_handle._1.memoryCellStack(3).state.low === 3)
    assert(state_and_handle._1.memoryCellStack(3).state.high === 3)

    assert(state_and_handle._1.memoryCellStack(2).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(2).state.index === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(2).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(2).state.handle === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.low === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.high === 3)

    state_and_handle = ESFAArrayOp().update(state_and_handle._1, Some(1), 9, 5)

    // Assert old arrays are intact
    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 2)
    assert(state_and_handle._1.memoryCellStack(0).state.handle === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.array_code === 0)
    assert(state_and_handle._1.memoryCellStack(1).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(1).state.index === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(1).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.low === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.high === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.handle === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.array_code === 1)
    assert(state_and_handle._1.memoryCellStack(3).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(3).state.index === 10)
    assert(state_and_handle._1.memoryCellStack(3).state.value === 21)
    assert(state_and_handle._1.memoryCellStack(3).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(3).state.handle === 3)
    assert(state_and_handle._1.memoryCellStack(3).state.low === 4)
    assert(state_and_handle._1.memoryCellStack(3).state.high === 4)
    assert(state_and_handle._1.memoryCellStack(3).state.array_code === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(2).state.index === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(2).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(2).state.handle === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.low === 3)
    assert(state_and_handle._1.memoryCellStack(2).state.high === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.array_code === 3)

    // assert updated Array looks fine
    assert(state_and_handle._1.memoryCellStack(4).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(4).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(4).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(4).state.index === 9)
    assert(state_and_handle._1.memoryCellStack(4).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(4).state.rank === 3)
    assert(state_and_handle._1.memoryCellStack(4).state.handle === 4)
    assert(state_and_handle._1.memoryCellStack(4).state.low === 2)
    assert(state_and_handle._1.memoryCellStack(4).state.high === 2)
    assert(state_and_handle._1.memoryCellStack(4).state.array_code === 2)

    state_and_handle = ESFAArrayOp().update(state_and_handle._1, Some(1), 11, 14)

    // Assert old arrays are intact and have ranges/codes updated
    assert(state_and_handle._1.memoryCellStack(0).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(0).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(0).state.index === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(0).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(0).state.low === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.high === 3)
    assert(state_and_handle._1.memoryCellStack(0).state.handle === 0)
    assert(state_and_handle._1.memoryCellStack(0).state.array_code === 0)
    assert(state_and_handle._1.memoryCellStack(1).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(1).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(1).state.index === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(1).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(1).state.low === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.high === 3)
    assert(state_and_handle._1.memoryCellStack(1).state.handle === 1)
    assert(state_and_handle._1.memoryCellStack(1).state.array_code === 1)
    assert(state_and_handle._1.memoryCellStack(3).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(3).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(3).state.index === 10)
    assert(state_and_handle._1.memoryCellStack(3).state.value === 21)
    assert(state_and_handle._1.memoryCellStack(3).state.rank === 2)
    assert(state_and_handle._1.memoryCellStack(3).state.handle === 3)
    assert(state_and_handle._1.memoryCellStack(3).state.low === 5)
    assert(state_and_handle._1.memoryCellStack(3).state.high === 5)
    assert(state_and_handle._1.memoryCellStack(3).state.array_code === 5)
    assert(state_and_handle._1.memoryCellStack(2).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(2).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(2).state.index === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.value === 10)
    assert(state_and_handle._1.memoryCellStack(2).state.rank === 1)
    assert(state_and_handle._1.memoryCellStack(2).state.handle === 2)
    assert(state_and_handle._1.memoryCellStack(2).state.low === 4)
    assert(state_and_handle._1.memoryCellStack(2).state.high === 5)
    assert(state_and_handle._1.memoryCellStack(2).state.array_code === 4)
    assert(state_and_handle._1.memoryCellStack(4).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(4).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(4).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(4).state.index === 9)
    assert(state_and_handle._1.memoryCellStack(4).state.value === 5)
    assert(state_and_handle._1.memoryCellStack(4).state.rank === 3)
    assert(state_and_handle._1.memoryCellStack(4).state.handle === 4)
    assert(state_and_handle._1.memoryCellStack(4).state.low === 3)
    assert(state_and_handle._1.memoryCellStack(4).state.high === 3)
    assert(state_and_handle._1.memoryCellStack(4).state.array_code === 3)

    // Assert new array looks fine
    assert(state_and_handle._1.memoryCellStack(5).state.arrDef === true)
    assert(state_and_handle._1.memoryCellStack(5).state.eltDef === true)
    assert(state_and_handle._1.memoryCellStack(5).state.mark === false)
    assert(state_and_handle._1.memoryCellStack(5).state.index === 11)
    assert(state_and_handle._1.memoryCellStack(5).state.value === 14)
    assert(state_and_handle._1.memoryCellStack(5).state.rank === 3)
    assert(state_and_handle._1.memoryCellStack(5).state.handle === 5)
    assert(state_and_handle._1.memoryCellStack(5).state.low === 2)
    assert(state_and_handle._1.memoryCellStack(5).state.high === 2)
    assert(state_and_handle._1.memoryCellStack(5).state.array_code === 2)

    state_and_handle = ESFAArrayOp().delete(state_and_handle._1, 1)






  }
}