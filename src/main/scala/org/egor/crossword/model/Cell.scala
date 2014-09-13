package org.egor.crossword.model

class Cell( state: Int, c:String, wd:Int) {
  def this(state:Int) = this(state, "", -1)
  def this() = this(CellState.ANY_DIRECTION)

  var avaliability = state
  var char = c
  var wordDirection = wd

  override def toString():String = {
    if (avaliability == CellState.WORD) s"|$char|"
    else if(avaliability == CellState.ANY_DIRECTION) "|-|"
//    else s"|$avaliability|"
    else "|-|"
  }
}


object CellState {
 val FORBIDDEN_DIRECTION = 9
 val ANY_DIRECTION = 0
 val HORIZONTAL_DIRECTION = 1
 val VERTICAL_DIRECTION = 2
 val WORD = 3

}
