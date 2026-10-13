//> using scala 2.12.20
//> using lib "com.beachape::enumeratum:1.9.0"

import enumeratum.EnumEntry

trait Months[V] {
  def Jan: V
  def Feb: V
  def Mar: V
  def Apr: V
  def May: V
  def Jun: V
  def Jul: V
  def Aug: V
  def Sep: V
  def Oct: V
  def Nov: V
  def Dec: V
}

sealed trait MyEnum extends EnumEntry

object MyEnum extends enumeratum.Enum[MyEnum] with Months[MyEnum] {
  val values = findValues

  case object Apr extends MyEnum
  case object Aug extends MyEnum
  case object Dec extends MyEnum
  case object Feb extends MyEnum
  case object Jan extends MyEnum
  case object Jul extends MyEnum
  case object Jun extends MyEnum
  case object Mar extends MyEnum
  case object May extends MyEnum
  case object Nov extends MyEnum
  case object Oct extends MyEnum
  case object Sep extends MyEnum

  implicit object myEnumIndex extends Months[Int] {
    def Jan = 1
    def Feb = 2
    def Mar = 3
    def Apr = 4
    def May = 5
    def Jun = 6
    def Jul = 7
    def Aug = 8
    def Sep = 9
    def Oct = 10
    def Nov = 11
    def Dec = 12
  }
}
