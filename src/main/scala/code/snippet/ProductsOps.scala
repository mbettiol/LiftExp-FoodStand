package code.snippet

import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Text

import code.model.Product
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.jquery.JqJsCmds
import net.liftweb.mapper.Descending
import net.liftweb.mapper.OrderBy
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.util.Helpers.strToCssBindPromoter

object ProductsOps {

  case class BindOp (nodeSeq : NodeSeq, jsCmd : JsCmd)
  
  private object productRV extends RequestVar(Product.create)
  private object listTamplateRV extends RequestVar[NodeSeq](Nil)
  private object formTemplateRV extends RequestVar[NodeSeq](Nil)
  private object guidToIdRV extends RequestVar[Map[String,Long]](Map())
  
  private val TEMPLATE_EDIT_ID = "edit-product-form";
  private val TEMPLATE_LIST_ID = "products-list";
  
  //
  // Template binding snippet methods are called in LiftSession#processSnippet
  //
  // There are 2 main approaches:
  //  1- method(in: NodeSeq) : NodeSeq                      - used here to keep a reference to the binded template node
  //  2- method() : CssSel or any (in:NodeSeq) => NodeSeq   - seems to be the best practice right now
  //
  
  /**
   * form template binding
   */
  def edit(in: NodeSeq) : NodeSeq = {
	  formTemplateRV.set(in)
	 _form_reset.nodeSeq
  }
  
  /**
   * list template binding
   */
  def list(in:NodeSeq) : NodeSeq = {
    val products = Product.findAll(OrderBy(Product.name, Descending))                       
    listTamplateRV(in)
    _rowTemplate(products);
  }
  
  private def _form_reset : BindOp = {
    val p = Product.create;
    _ajaxFormEdit(p, true);
  }
  
  private def _ajaxFormEdit(p : Product, create : Boolean) : BindOp = {
    val old = productRV.is
    val deselectCmd = if(old.saved_?) _ajaxRenderRow(p,false,false) else JsCmds.Noop
    productRV.set(p);
    val form = _formTemplate(p, create);
    val jsCmd = JsCmds.Replace(TEMPLATE_EDIT_ID,form);
    BindOp(form,jsCmd & deselectCmd)
  }
  
  private def _ajaxDelete(p: Product, guid: String ) : JsCmd = {
    guidToIdRV.set(guidToIdRV.is-guid)
    p.delete_!; 
    JsCmds.Replace(guid,NodeSeq.Empty) & _form_reset.jsCmd
  }
  
  private def processSubmit() : JsCmd = {
	productRV.is.validate match {
	case Nil => {
		val isNew = !productRV.is.saved_?
		val p = productRV.is.saveMe
		S.notice("Product Saved")
		_ajaxRenderRow(p, isNew, false) & 
		_form_reset.jsCmd
	} 
	case errors => 
		S.error(errors)
		JsCmds.Noop
	}
  }
  
  private def _ajaxRenderRow(p : Product,isNew: Boolean, selected : Boolean) : JsCmd = {
    val templateRowRoot = TEMPLATE_LIST_ID;
	val xml =_rowTemplate(List(productRV.is))
	var op : Option[JsCmd] = None;
	for { //extract "tr" from the generated html, 1 and only 1 tr exists
		elem <- xml \\ "_"
		tr <- (elem \ "tr") if (elem\"@id").text == templateRowRoot
	} yield {
		val ajaxRow = if (selected) ("td [class+]" #> "selected-row").apply(tr) else tr
		if(isNew){
			op = Some(JqJsCmds.AppendHtml(templateRowRoot,ajaxRow)); //append to current content
		}else{
			val guid = associatedGuid(productRV.is.id.get).get
			op = Some(JsCmds.Replace(guid,ajaxRow));//replace inner content
	}
	}
	op.get //never None
  }

  private def associatedGuid(l : Long) : Option[String] = {
    val map =guidToIdRV.is;
    map.find(e => l==e._2) match {
      case Some(e)  =>  Some(e._1)
      case None => 
      		val guid = nextFuncName
      		guidToIdRV.set(map+(guid ->l))
      		Some(guid)
     }
  }
  
  private def _rowTemplate (products : List[Product]) : NodeSeq = {
    val in = listTamplateRV.is;
    val cssSel = 
    ".row" #> products.map( p =>
    {
      val guid = associatedGuid(p.id.is).get
      ".row [id]" #> (guid) &
	  cellSelector("name") #> Text(p.name.is) &                
	  cellSelector("price") #> Text(p.price.is.toString) &
	  cellSelector("enabled") #> Text(p.enabled.toString) &
	  cellSelector("actions") #> { 
	      SHtml.a(() => {_ajaxFormEdit(p,false).jsCmd & _ajaxRenderRow(p, false, true)}, Text("edit")) ++
	      Text(" | ") ++  
	      SHtml.a(() => { _ajaxDelete(p, guid)}, Text("delete"))
	      }
	})
	cssSel.apply(in)
  }
  
  private def cellSelector(p : String) : String = {
    ".product-"+p+" *"
  }
  
  private def _formTemplate (p: Product, create : Boolean) : NodeSeq = {
    val in = formTemplateRV.is;
    val cssSel =  
      "#product-page-title *" #> (if(create)  "Create Product" else "Update Product") & 
      "#name" #> SHtml.text(productRV.name.is,   name => productRV.is.name(name)) &
      "#price" #> SHtml.number(productRV.price.is,   (price : Double) => productRV.is.price(price) ,0,100,0.5) &
      "#enabled" #> SHtml.checkbox(productRV.enabled.is, (b) => productRV.is.enabled(b)) &
      "#submit" #> SHtml.ajaxSubmit("Save", processSubmit) &
      "#reset" #> SHtml.ajaxSubmit("Reset", () =>  _form_reset.jsCmd)
    cssSel.apply(in)
  }
  
}