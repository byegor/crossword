package org.egor.crossword.model

class CrossWord() {

  val variants: List[Array[Array[Cell]]] = Nil

  def createField(size: Int) = {
    Array.fill(size, size)(new Cell())
  }

  def generateCrossWord(words: List[String], dimension: Int): Unit = {
    val sortedWords: List[String] = words.filter(x => x.length <= dimension).sortBy(x => -x.length)
    val field: Array[Array[Cell]] = createField(dimension)
    setTheFirstWord(sortedWords.head, field)
    sortedWords.tail.foreach(setWord(field, _))
    printArray(field)
  }

  private def printArray(newField: Array[Array[Cell]]): Unit = {
    for (i <- 0 until newField.length) {
      for (j <- 0 until newField.length) {
        print(newField(i)(j) + " ")
      }
      println()
    }
  }

  def setTheFirstWord(w: String, f: Array[Array[Cell]]) = {
    val startPos: Int = (f.length - w.length) / 2
    val vertPos: Int = f.length / 2 - 1
    placeTheWord(f, vertPos, startPos, w, CellState.HORIZONTAL_DIRECTION)

    f
  }

  def placeTheWord(f: Array[Array[Cell]], startI: Int, startJ: Int, word: String, wordDirection: Int) = {
    if (CellState.HORIZONTAL_DIRECTION == wordDirection) {
      var index = 0
      for (x <- startJ until startJ + word.length) {
        f(startI)(x) = new Cell(CellState.WORD, word(index).toString, wordDirection)
        setNeighbours(f, startI, x, wordDirection)
        index = index + 1
      }
    } else {
      var index = 0
      for (x <- startI until startI + word.length) {
        f(x)(startJ) = new Cell(CellState.WORD, word(index).toString, wordDirection)
        setNeighbours(f, x, startJ, wordDirection)
        index = index + 1
      }
    }
    setStartAndFinish(f, startI, startJ, wordDirection, word.length)
  }

  def setStartAndFinish(f: Array[Array[Cell]], i: Int, j: Int, direction: Int, wordLength: Int): Unit = {
    if (direction == CellState.VERTICAL_DIRECTION) {
      if (i - 1 >= 0) {
        if (f(i - 1)(j).avaliability != CellState.WORD) f(i - 1)(j).avaliability = CellState.FORBIDDEN_DIRECTION
        if (j - 1 >= 0) f(i - 1)(j - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (j + 1 <= f.length) f(i - 1)(j + 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
      if (i + wordLength <= f.length) {
        f(i + wordLength)(j).avaliability = CellState.FORBIDDEN_DIRECTION
        if (j - 1 >= 0) f(i + wordLength)(j - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (j + 1 <= f.length) f(i + wordLength)(j + 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
    } else {
      if (j - 1 >= 0) {
        f(i)(j - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (i - 1 >= 0) f(i - 1)(j - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (i + 1 <= f.length) f(i + 1)(j - 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
      if (j + wordLength < f.length) {
        f(i)(j + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
        if (i - 1 >= 0) f(i - 1)(j + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
        if (i + 1 <= f.length) f(i + 1)(j + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
      }
    }
  }

  def setNeighbours(f: Array[Array[Cell]], i: Int, j: Int, direction: Int): Unit = {
    def setVerticalAvaliabilityToNehgbours(x: Int, y: Int) {
      val c: Cell = f(x)(y)
      if (c.avaliability != CellState.WORD)
        if (c.avaliability == CellState.HORIZONTAL_DIRECTION) {
          c.avaliability = CellState.FORBIDDEN_DIRECTION
        } else {
          c.avaliability = CellState.VERTICAL_DIRECTION
        }
    }
    def setHorizontalAvaliabilityToNehgbours(x: Int, y: Int) {
      val c: Cell = f(x)(y)
      if (c.avaliability != CellState.WORD)
        if (c.avaliability == CellState.VERTICAL_DIRECTION) {
          c.avaliability = CellState.FORBIDDEN_DIRECTION
        } else {
          c.avaliability = CellState.HORIZONTAL_DIRECTION
        }
    }

    if (direction == CellState.HORIZONTAL_DIRECTION) {
      if (i - 1 >= 0) setVerticalAvaliabilityToNehgbours(i - 1, j)
      if (i + 1 < f.length) setVerticalAvaliabilityToNehgbours(i + 1, j)
    } else {
      if (j - 1 >= 0) setHorizontalAvaliabilityToNehgbours(i, j - 1)
      if (j + 1 < f.length) setHorizontalAvaliabilityToNehgbours(i, j + 1)
    }
  }

  def setWord(f: Array[Array[Cell]], word: String): Array[Array[Cell]] = {
    for (i <- 0 until f.length) {
      for (j <- 0 until f.length) {
        val cell: Cell = f(i)(j)
        if (cell.avaliability == CellState.WORD) {
          if (word.contains(cell.char)) {
            val index: Int = word.indexOf(cell.char)
            if (cell.wordDirection == CellState.HORIZONTAL_DIRECTION) {
              val startPos: Int = i - index
              if (checkAvaliability(f, startPos, j, word, CellState.VERTICAL_DIRECTION)) {
                placeTheWord(f, startPos, j, word, CellState.VERTICAL_DIRECTION)
                return f
              }
            } else {
              val startPos: Int = j - index
              if (checkAvaliability(f, startPos, i, word, CellState.HORIZONTAL_DIRECTION)) {
                placeTheWord(f, i, startPos, word, CellState.HORIZONTAL_DIRECTION)
                return f
              }
            }
          }
        }
      }
    }
    f
  }


  def checkAvaliability(f: Array[Array[Cell]], startPos: Int, rowOrColumn: Int, word: String, wordDirection: Int): Boolean = {
    var result = startPos >= 0 && startPos + word.length <= f.length
    if (result) {
      if (wordDirection == CellState.HORIZONTAL_DIRECTION) {
        for (k <- 0 until word.length) {
          val cell: Cell = f(rowOrColumn)(k + startPos)
          if (cell.avaliability == CellState.WORD) {
            if (cell.char != word(k).toString) result = false
          } else if (cell.avaliability != CellState.HORIZONTAL_DIRECTION && cell.avaliability != CellState.ANY_DIRECTION) {
            result = false
          }
        }
      } else {
        for (k <- 0 until word.length) {
          val cell: Cell = f(k + startPos)(rowOrColumn)
          if (cell.avaliability == CellState.WORD) {
            if (cell.char != word(k).toString) result = false
          } else if (cell.avaliability != CellState.VERTICAL_DIRECTION && cell.avaliability != CellState.ANY_DIRECTION) {
            result = false
          }
        }
      }
    }
    result
  }
}


object Cross {
  def main(s: Array[String]): Unit = {
    val crossWord: CrossWord = new CrossWord()
    crossWord.generateCrossWord(s.toList, 16)

  }
}