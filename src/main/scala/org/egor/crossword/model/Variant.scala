package org.egor.crossword.model

class Variant(f: Array[Array[Cell]], c:Int) {

  val wordField = f
  var wordCount = c
  val length = f.length

  def wordPlaced(): Unit ={
    wordCount += 1
  }

  override def clone(): Variant = new Variant(wordField.map(_.map(_.clone())), wordCount)
}
