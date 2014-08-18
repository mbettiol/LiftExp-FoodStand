package code
package model

import net.liftweb.mapper._

class Product extends LongKeyedMapper[Product] with IdPK {

	enabled(true) //default
  
	def getSingleton = Product
	
	object name extends MappedString(this, 140) {
	  override def validations = { valMinLen(3, "name : min 3") _ :: super.validations}
	}
	object price extends MappedDouble(this)
	object enabled extends MappedBoolean(this)
	object productType extends MappedLongForeignKey(this, ProductType) //TODO NYI

}

object Product extends Product with LongKeyedMetaMapper[Product]{
  

}

class ProductType extends LongKeyedMapper[ProductType] with IdPK{
  
  def getSingleton = ProductType
  
  object name extends MappedString(this,80)
  
}

object ProductType extends ProductType with LongKeyedMetaMapper[ProductType]{
  
}




//object TipoPiatto extends Enumeration{
//  val Primo = Value("Primo") 
//  val Secondo = Value("Secondo") 
//  val Contorno = Value("Contorno") 
//  val Dolce = Value("Dolce") 
//}