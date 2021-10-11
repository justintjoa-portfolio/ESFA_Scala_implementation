import ESFAArray.Array.Op.ESFAArrayOp
import ESFAArray.Array.State.ESFAArrayState
import org.scalatest.FunSuite

class Test extends FunSuite {

  test("UpdateBasicTest") {
    val emptyArrayState: ESFAArrayState = ESFAArrayState()
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
        fail("Should be updating and adding a legitimate entry with handle\n")
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
        fail("Should be updating and adding a legitimate entry with handle\n")
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

    ESFAArrayOp().lookUp(state_and_handle._1, 0, 0) match {
      case Right(value) => {
        assert(value === 5)
      }
      case Left(error_message) => {
        print(error_message)
        fail("could not find first array\n")
      }
    }

    ESFAArrayOp().lookUp(state_and_handle._1, 1, 0) match {
      case Right(value) => {
        assert(value === 5)
      }
      case Left(error_message) => {
        print(error_message)
        fail("could not find first array in second array\n")
      }
    }

    ESFAArrayOp().lookUp(state_and_handle._1, 1, 2) match {
      case Right(value) => {
        assert(value === 10)
      }
      case Left(error_message) => {
        print(error_message)
        fail("could not find first array in second array\n")
      }
    }

    ESFAArrayOp().lookUp(state_and_handle._1, 3, 4) match {
      case Right(value) => {
        assert(value === 10)
      }
      case Left(error_message) => {
        print(error_message)
        fail("could not find third array in fourth array\n")
      }
    }

    ESFAArrayOp().lookUp(state_and_handle._1, 5, 2) match {
      case Right(value) => {
        assert(value === 10)
      }
      case Left(error_message) => {
        print(error_message)
        fail("could not find second array in fifth array\n")
      }
    }

    ESFAArrayOp().lookUp(state_and_handle._1, 5, 1) match {
      case Right(value) => {
        fail("You shouldn't have a value with index 1 in sixth array\n")
      }
      case Left(error_message) => {
        print(error_message)
        print("Success - you should in fact have an error here, as there is no value with index 1 in sixth array.\n")
      }
    }

    val (_, new_state) = ESFAArrayOp().delete(state_and_handle._1, 1)
    assert(new_state.memoryCellStack(0).state.arrDef === true)
    assert(new_state.memoryCellStack(0).state.eltDef === true)
    assert(new_state.memoryCellStack(0).state.mark === false)
    assert(new_state.memoryCellStack(0).state.index === 0)
    assert(new_state.memoryCellStack(0).state.value === 5)
    assert(new_state.memoryCellStack(0).state.rank === 1)
    assert(new_state.memoryCellStack(0).state.handle === 0)
    assert(new_state.memoryCellStack(0).state.low === 0)
    assert(new_state.memoryCellStack(0).state.high === 2)
    assert(new_state.memoryCellStack(0).state.array_code === 0)

    assert(new_state.memoryCellStack(1).state.arrDef === false)
    assert(new_state.memoryCellStack(1).state.eltDef === true)
    assert(new_state.memoryCellStack(1).state.mark === false)
    assert(new_state.memoryCellStack(1).state.index === 2)
    assert(new_state.memoryCellStack(1).state.value === 10)
    assert(new_state.memoryCellStack(1).state.rank === 0)
    assert(new_state.memoryCellStack(1).state.handle === 1)
    assert(new_state.memoryCellStack(1).state.low === 1)
    assert(new_state.memoryCellStack(1).state.high === 2)
    assert(new_state.memoryCellStack(1).state.array_code === 1)

    assert(new_state.memoryCellStack(2).state.arrDef === true)
    assert(new_state.memoryCellStack(2).state.eltDef === true)
    assert(new_state.memoryCellStack(2).state.mark === false)
    assert(new_state.memoryCellStack(2).state.index === 4)
    assert(new_state.memoryCellStack(2).state.value === 10)
    assert(new_state.memoryCellStack(2).state.rank === 1)
    assert(new_state.memoryCellStack(2).state.handle === 2)
    assert(new_state.memoryCellStack(2).state.low === 3)
    assert(new_state.memoryCellStack(2).state.high === 4)
    assert(new_state.memoryCellStack(2).state.array_code === 3)

    assert(new_state.memoryCellStack(3).state.arrDef === true)
    assert(new_state.memoryCellStack(3).state.eltDef === true)
    assert(new_state.memoryCellStack(3).state.mark === false)
    assert(new_state.memoryCellStack(3).state.index === 10)
    assert(new_state.memoryCellStack(3).state.value === 21)
    assert(new_state.memoryCellStack(3).state.rank === 2)
    assert(new_state.memoryCellStack(3).state.handle === 3)
    assert(new_state.memoryCellStack(3).state.low === 4)
    assert(new_state.memoryCellStack(3).state.high === 4)
    assert(new_state.memoryCellStack(3).state.array_code === 4)

    assert(new_state.memoryCellStack(4).state.arrDef === true)
    assert(new_state.memoryCellStack(4).state.eltDef === true)
    assert(new_state.memoryCellStack(4).state.mark === false)
    assert(new_state.memoryCellStack(4).state.index === 9)
    assert(new_state.memoryCellStack(4).state.value === 5)
    assert(new_state.memoryCellStack(4).state.rank === 3)
    assert(new_state.memoryCellStack(4).state.handle === 4)
    assert(new_state.memoryCellStack(4).state.low === 2)
    assert(new_state.memoryCellStack(4).state.high === 2)
    assert(new_state.memoryCellStack(4).state.array_code === 2)

    assert(new_state.memoryCellStack(5).state.arrDef === true)
    assert(new_state.memoryCellStack(5).state.eltDef === true)
    assert(new_state.memoryCellStack(5).state.mark === false)
    assert(new_state.memoryCellStack(5).state.index === 11)
    assert(new_state.memoryCellStack(5).state.value === 14)
    assert(new_state.memoryCellStack(5).state.rank === 3)
    assert(new_state.memoryCellStack(5).state.handle === 5)
    assert(new_state.memoryCellStack(5).state.low === 1)
    assert(new_state.memoryCellStack(5).state.high === 1)
    assert(new_state.memoryCellStack(5).state.array_code === 1)

    var post_deletion_state = new_state
    post_deletion_state = ESFAArrayOp().delete(post_deletion_state, 4)._2
    post_deletion_state = ESFAArrayOp().delete(post_deletion_state, 5)._2

    assert(post_deletion_state.memoryCellStack(0).state.arrDef === true)
    assert(post_deletion_state.memoryCellStack(0).state.eltDef === true)
    assert(post_deletion_state.memoryCellStack(0).state.mark === false)
    assert(post_deletion_state.memoryCellStack(0).state.index === 0)
    assert(post_deletion_state.memoryCellStack(0).state.value === 5)
    assert(post_deletion_state.memoryCellStack(0).state.rank === 1)
    assert(post_deletion_state.memoryCellStack(0).state.handle === 0)
    assert(post_deletion_state.memoryCellStack(0).state.low === 0)
    assert(post_deletion_state.memoryCellStack(0).state.high === 0)
    assert(post_deletion_state.memoryCellStack(0).state.array_code === 0)

    assert(post_deletion_state.memoryCellStack(1).state.arrDef === false)
    assert(post_deletion_state.memoryCellStack(1).state.eltDef === false)
    assert(post_deletion_state.memoryCellStack(1).state.mark === false)

    assert(post_deletion_state.memoryCellStack(2).state.arrDef === true)
    assert(post_deletion_state.memoryCellStack(2).state.eltDef === true)
    assert(post_deletion_state.memoryCellStack(2).state.mark === false)
    assert(post_deletion_state.memoryCellStack(2).state.index === 4)
    assert(post_deletion_state.memoryCellStack(2).state.value === 10)
    assert(post_deletion_state.memoryCellStack(2).state.rank === 1)
    assert(post_deletion_state.memoryCellStack(2).state.handle === 2)
    assert(post_deletion_state.memoryCellStack(2).state.low === 1)
    assert(post_deletion_state.memoryCellStack(2).state.high === 2)
    assert(post_deletion_state.memoryCellStack(2).state.array_code === 1)

    assert(post_deletion_state.memoryCellStack(3).state.arrDef === true)
    assert(post_deletion_state.memoryCellStack(3).state.eltDef === true)
    assert(post_deletion_state.memoryCellStack(3).state.mark === false)
    assert(post_deletion_state.memoryCellStack(3).state.index === 10)
    assert(post_deletion_state.memoryCellStack(3).state.value === 21)
    assert(post_deletion_state.memoryCellStack(3).state.rank === 2)
    assert(post_deletion_state.memoryCellStack(3).state.handle === 3)
    assert(post_deletion_state.memoryCellStack(3).state.low === 2)
    assert(post_deletion_state.memoryCellStack(3).state.high === 2)
    assert(post_deletion_state.memoryCellStack(3).state.array_code === 2)

    assert(post_deletion_state.memoryCellStack(4).state.arrDef === false)
    assert(post_deletion_state.memoryCellStack(4).state.eltDef === false)
    assert(post_deletion_state.memoryCellStack(4).state.mark === false)

    assert(post_deletion_state.memoryCellStack(5).state.arrDef === false)
    assert(post_deletion_state.memoryCellStack(5).state.eltDef === false)
    assert(post_deletion_state.memoryCellStack(5).state.mark === false)

    ESFAArrayOp().lookUp(post_deletion_state, 0, 0) match {
      case Right(value) => {
        assert(value === 5)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Could not find first array\n")
      }
    }
  }

  test("ESFATransversalTest") {
    print("start transversal test\n")
    val emptyArrayState: ESFAArrayState = ESFAArrayState()

    var transversal_state = emptyArrayState
    transversal_state = ESFAArrayOp().update(transversal_state, None, 0, 10)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(0), 3, 14)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(1), 2, 9)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(2), 20, 21)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(3), 15, 7)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(2), 29, 7)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(4), 95, 23)._1
    transversal_state = ESFAArrayOp().update(transversal_state, Some(6), 18, 7)._1

    ESFAArrayOp().minDef(transversal_state, 7) match {
      case Right(defStats) => {
        assert(defStats._1 === 0)
        assert(defStats._2 === 0)
        assert(defStats._3 === 10)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

    ESFAArrayOp().nextDef(transversal_state, 7, 0) match {
      case Right(defStats) => {
        assert(defStats._1 === 2)
        assert(defStats._2 === 2)
        assert(defStats._3 === 9)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

    ESFAArrayOp().nextDef(transversal_state, 7, 2) match {
      case Right(defStats) => {
        assert(defStats._1 === 1)
        assert(defStats._2 === 3)
        assert(defStats._3 === 14)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

    ESFAArrayOp().prevDef(transversal_state, 7, 3) match {
      case Right(defStats) => {
        assert(defStats._1 === 2)
        assert(defStats._2 === 2)
        assert(defStats._3 === 9)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

    ESFAArrayOp().maxDef(transversal_state, 7) match {
      case Right(defStats) => {
        assert(defStats._1 === 6)
        assert(defStats._2 === 95)
        assert(defStats._3 === 23)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

    ESFAArrayOp().prevDef(transversal_state, 7, 95) match {
      case Right(defStats) => {
        assert(defStats._1 === 3)
        assert(defStats._2 === 20)
        assert(defStats._3 === 21)
      }
      case Left(error_message) => {
        print(error_message)
        fail("Did not find def\n")
      }
    }

  }
}