package org.egor.crossword.model

class CrossWord() {

  var variants: List[Variant] = Nil

  def createField(size: Int) = {
    Array.fill(size, size)(new Cell())
  }

  def generateCrossWord(words: List[String], dimension: Int): Unit = {
    val sortedWords: List[String] = words.filter(x => x.length <= dimension).sortBy(x => -x.length)
    val field: Array[Array[Cell]] = createField(dimension)
    val v:Variant = new Variant(field, 0)
    variants = setTheFirstWord(sortedWords.head, v) ::: Nil
    
    for(i  <- 1 until sortedWords.length){
    val word: String = sortedWords(i)
      var newVariants :List[Variant]= Nil
      for(k<-0 until variants.length){
        val crossField: Variant = variants(k)
        val v: List[Variant] = setWord(crossField, word)
        newVariants =  v ::: newVariants
      }
      variants = newVariants
    }
    variants.sortBy(_.wordCount).foreach(printArray)
  }

  private def printArray(newField: Variant): Unit = {
    println("<==============" + newField.wordCount + "==============>")

    for (i <- 0 until newField.length) {
      for (j <- 0 until newField.length) {
        print(newField.wordField(i)(j) + " ")
      }
      println()
    }
    println()
    println()
    println()
  }

  def setTheFirstWord(w: String, v: Variant) = {
    val startPos: Int = (v.wordField.length - w.length) / 2
    val vertPos: Int = v.wordField.length / 2 - 1

    val v2: Variant = v.clone()
    placeTheWord(v, vertPos, startPos, w, CellState.HORIZONTAL_DIRECTION)
    placeTheWord(v2, startPos, vertPos, w, CellState.VERTICAL_DIRECTION)
    List(v, v2)
  }

  def cloneArray(f: Array[Array[Cell]])={
    f.map(_.map(_.clone()))
  }

  def placeTheWord(variant:Variant, startI: Int, startJ: Int, word: String, wordDirection: Int) = {
    if (CellState.HORIZONTAL_DIRECTION == wordDirection) {
      var index = 0
      for (x <- startJ until startJ + word.length) {
        variant.wordField(startI)(x) = new Cell(CellState.WORD, word(index).toString, wordDirection)
        setNeighbours(variant, startI, x, wordDirection)
        index = index + 1
      }
    } else {
      var index = 0
      for (x <- startI until startI + word.length) {
        variant.wordField(x)(startJ) = new Cell(CellState.WORD, word(index).toString, wordDirection)
        setNeighbours(variant, x, startJ, wordDirection)
        index = index + 1
      }
    }
    setStartAndFinish(variant, startI, startJ, wordDirection, word.length)
    variant.wordPlaced()
    variant
  }

  private def setStartAndFinish(variant:Variant, startI: Int, startJ: Int, direction: Int, wordLength: Int): Unit = {
    val f = variant.wordField
    if (direction == CellState.VERTICAL_DIRECTION) {
      if (startI - 1 >= 0) {
        f(startI - 1)(startJ).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startJ - 1 >= 0) f(startI - 1)(startJ - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startJ + 1 < f.length) f(startI - 1)(startJ + 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
      if (startI + wordLength < f.length) {
        f(startI + wordLength)(startJ).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startJ - 1 >= 0) f(startI + wordLength)(startJ - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startJ + 1 < f.length) f(startI + wordLength)(startJ + 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
    } else {
      if (startJ - 1 >= 0) {
        f(startI)(startJ - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startI - 1 >= 0) f(startI - 1)(startJ - 1).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startI + 1 < f.length) f(startI + 1)(startJ - 1).avaliability = CellState.FORBIDDEN_DIRECTION
      }
      if (startJ + wordLength < f.length) {
        f(startI)(startJ + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startI - 1 >= 0) f(startI - 1)(startJ + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
        if (startI + 1 < f.length) f(startI + 1)(startJ + wordLength).avaliability = CellState.FORBIDDEN_DIRECTION
      }
    }
  }

  private def checkStarnAndFinishForAvalaibility(field: Array[Array[Cell]], startI: Int, startJ: Int, direction: Int, wordLength: Int): Boolean = {
    if (direction == CellState.VERTICAL_DIRECTION) {
      if (startI - 1 >= 0) {
        if (!field(startI - 1)(startJ).isAvaliabile()) return false
        if (startJ - 1 >= 0 && !field(startI - 1)(startJ - 1).isAvaliabile()) return false
        if (startJ + 1 < field.length && !field(startI - 1)(startJ + 1).isAvaliabile()) return false
      }
      if (startI + wordLength < field.length) {
        if (field(startI + wordLength)(startJ).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI + wordLength)(startJ).avaliability == CellState.WORD) return false
        if (startJ - 1 >= 0 && (field(startI + wordLength)(startJ - 1).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI + wordLength)(startJ - 1).avaliability == CellState.WORD)) return false
        if (startJ + 1 < field.length && (field(startI + wordLength)(startJ + 1).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI + wordLength)(startJ + 1).avaliability == CellState.WORD)) return false
      }
    } else {
      if (startJ - 1 >= 0) {
        if( field(startI)(startJ - 1).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI)(startJ - 1).avaliability == CellState.WORD) return false
        if (startI - 1 >= 0 && (field(startI - 1)(startJ - 1).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI - 1)(startJ - 1).avaliability == CellState.WORD)) return false
        if (startI + 1 < field.length && (field(startI + 1)(startJ - 1).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI + 1)(startJ - 1).avaliability == CellState.WORD)) return false
      }
      if (startJ + wordLength < field.length) {
        if (field(startI)(startJ + wordLength).avaliability == CellState.FORBIDDEN_DIRECTION || field(startI)(startJ + wordLength).avaliability == CellState.WORD) return  false
        if (startI - 1 >= 0 && !field(startI - 1)(startJ + wordLength).isAvaliabile()) return false
        if (startI + 1 < field.length && !field(startI + 1)(startJ + wordLength).isAvaliabile()) return false
      }
    }
    true
  }

  def setNeighbours(variant:Variant, i: Int, j: Int, direction: Int): Unit = {
    def setVerticalAvaliabilityToNehgbours(x: Int, y: Int) {
      val c: Cell = variant.wordField(x)(y)
      if (c.avaliability != CellState.WORD)
        if (c.avaliability == CellState.HORIZONTAL_DIRECTION) {
          c.avaliability = CellState.FORBIDDEN_DIRECTION
        } else {
          c.avaliability = CellState.VERTICAL_DIRECTION
        }
    }
    def setHorizontalAvaliabilityToNehgbours(x: Int, y: Int) {
      val c: Cell = variant.wordField(x)(y)
      if (c.avaliability != CellState.WORD)
        if (c.avaliability == CellState.VERTICAL_DIRECTION) {
          c.avaliability = CellState.FORBIDDEN_DIRECTION
        } else {
          c.avaliability = CellState.HORIZONTAL_DIRECTION
        }
    }

    if (direction == CellState.HORIZONTAL_DIRECTION) {
      if (i - 1 >= 0) setVerticalAvaliabilityToNehgbours(i - 1, j)
      if (i + 1 < variant.wordField.length) setVerticalAvaliabilityToNehgbours(i + 1, j)
    } else {
      if (j - 1 >= 0) setHorizontalAvaliabilityToNehgbours(i, j - 1)
      if (j + 1 < variant.wordField.length) setHorizontalAvaliabilityToNehgbours(i, j + 1)
    }
  }

  def setWord(variant: Variant, word: String): List[Variant] = {
    var result:List[Variant] = Nil
    for (i <- 0 until variant.length) {
      for (j <- 0 until variant.length) {
        val cell: Cell = variant.wordField(i)(j)
        if (cell.avaliability == CellState.WORD) {
          if (word.contains(cell.char)) {
            val index: Int = word.indexOf(cell.char)
            if (cell.wordDirection == CellState.HORIZONTAL_DIRECTION) {
              val startPos: Int = i - index
              if (checkAvaliability(variant.wordField, startPos, j, word, CellState.VERTICAL_DIRECTION)) {
                val clone = placeTheWord(variant.clone(), startPos, j, word, CellState.VERTICAL_DIRECTION)
                result = List(clone) ::: result
              }
            } else {
              val startPos: Int = j - index
              if (checkAvaliability(variant.wordField, startPos, i, word, CellState.HORIZONTAL_DIRECTION)) {
                val clone: Variant = placeTheWord(variant.clone(), i, startPos, word, CellState.HORIZONTAL_DIRECTION)
                result = List(clone) ::: result
              }
            }
          }
        }
      }
    }
    if (result.isEmpty) List(variant) else result
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
        result = result && checkStarnAndFinishForAvalaibility(f,rowOrColumn, startPos, wordDirection, word.length)
      } else {
        for (k <- 0 until word.length) {
          val cell: Cell = f(k + startPos)(rowOrColumn)
          if (cell.avaliability == CellState.WORD) {
            if (cell.char != word(k).toString) result = false
          } else if (cell.avaliability != CellState.VERTICAL_DIRECTION && cell.avaliability != CellState.ANY_DIRECTION) {
            result = false
          }
        }
        result = result && checkStarnAndFinishForAvalaibility(f, startPos, rowOrColumn, wordDirection, word.length)
      }
    }
    result
  }
}
