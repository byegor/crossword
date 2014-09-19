package org.egor.crossword.model

import org.scalatest.FunSuite

class WordTest extends FunSuite {

  test("set First word") {
    val word: CrossWord = new CrossWord()
    val filed: Array[Array[Cell]] = word.createField(6)
    val v:Variant = new Variant(filed, 0)
    val variations: List[Variant] = word.setTheFirstWord("abrd", v)
    val newField = variations(0).wordField
    val cell: Cell = newField(2)(1)
    assert(CellState.WORD == cell.avaliability)
    assert("a" == cell.char)
    assert(CellState.HORIZONTAL_DIRECTION == cell.wordDirection)
    printArray(variations(0).wordField)
    printArray(variations(1).wordField)
    val cell1: Cell = newField(2)(4)
    assert(CellState.WORD == cell1.avaliability)
    assert("d" == cell1.char)

    val second = variations(1).wordField
    assert(CellState.WORD == second(1)(2).avaliability)
    assert("a" == second(1)(2).char)
    assert(CellState.VERTICAL_DIRECTION == second(1)(2).wordDirection)

  }

  def printArray(newField: Array[Array[Cell]]): Unit = {
    for (i <- 0 until newField.length) {
      for (j <- 0 until newField.length) {
        print(newField(i)(j) + " ")
      }
      println()
    }
  }
  /*
    test("set neighbours avaliability") {

      val word: CrossWord = new CrossWord()
      val filed: Array[Array[Cell]] = word.createField(6)
      val variations: List[Array[Array[Cell]]] = word.setTheFirstWord("abrd", filed)
      val newField: Array[Array[Cell]] = variations(0)
  printArray(variations(0))
  printArray(variations(1))
      var cell: Cell = newField(1)(1)
      assert(cell.avaliability == CellState.VERTICAL_DIRECTION)

      cell = newField(3)(4)
      assert(cell.avaliability == CellState.VERTICAL_DIRECTION)

    }

    test("set start and finish") {

      val word: CrossWord = new CrossWord()
      val filed: Array[Array[Cell]] = word.createField(6)
      val newField: Array[Array[Cell]] = word.setTheFirstWord("abrd", filed)(0)

      var cell: Cell = newField(2)(0)
      assert(cell.avaliability == CellState.FORBIDDEN_DIRECTION)

      cell = newField(2)(5)
      assert(cell.avaliability == CellState.FORBIDDEN_DIRECTION)
    }

    test("Check avaliable place for Word") {

      val cross: CrossWord = new CrossWord()
      val filed: Array[Array[Cell]] = cross.createField(6)
      val newField: Array[Array[Cell]] = cross.setTheFirstWord("abrd", filed)(0)

      assert(cross.checkAvaliability(newField, 1, 1, "dark", CellState.VERTICAL_DIRECTION))
      assert(!cross.checkAvaliability(newField, 1, 2, "dark", CellState.VERTICAL_DIRECTION))
    }

    test("set second Word") {

      val word: CrossWord = new CrossWord()
      val filed: Array[Array[Cell]] = word.createField(6)
      val newField: Array[Array[Cell]] = word.setTheFirstWord("abrd", filed)(0)

      val f: Array[Array[Cell]] = word.setWord(newField, "dark")(0)
      val cell: Cell = f(1)(1)
      assert(cell.char == "d")
      assert(f(4)(1).char == "k")
    }

    test("setting more words"){
      val word: CrossWord = new CrossWord()
      val filed: Array[Array[Cell]] = word.createField(16)
      val newField: Array[Array[Cell]] = word.setTheFirstWord("towerdefence", filed)(0)
      val firstCell: Cell = newField(7)(2)
      assert(firstCell.char == "t")

      val f: Array[Array[Cell]] = word.setWord(newField, "imaginarium")(0)
      assert(f(0)(6).char == "i")

      word.setWord(f, "interesting")
      assert(f(5)(5).char == "i")

      word.setWord(f, "teleport")
      assert(f(5)(5).char == "i")

      word.setWord(f, "story")
      printArray(newField)
    }



    def printArray(newField: Array[Array[Cell]]): Unit = {
      for (i <- 0 until newField.length) {
        for (j <- 0 until newField.length) {
          print(newField(i)(j) + " ")
        }
        println()
      }
    }
  */
}
