package org.egor.crossword.model

object Main {
  def main(s: Array[String]): Unit = {
    val crossWord: CrossWord = new CrossWord()
    crossWord.generateCrossWord(s.toList, 15)
  }
}
