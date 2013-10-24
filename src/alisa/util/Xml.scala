package alisa.util

import org.w3c.dom.{NodeList, Node}
import javax.xml.xpath.{XPathConstants, XPathFactory, XPathExpression}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.{OutputKeys, TransformerFactory}
import java.io.StringWriter
import javax.xml.transform.stream.StreamResult

object Xml {

	type XmlNode = Node
	type Xpath = XPathExpression

	def xmlNodeText(node: XmlNode): String = node.getTextContent

	def toDomSource(node: XmlNode): DOMSource = new DOMSource(node)

	def xpc(s: String): Xpath = XPathFactory.newInstance.newXPath.compile(s)

	def evalXpathNodeList(exe: Xpath, node: XmlNode): Iterable[XmlNode] = {
		val nodeList = exe.evaluate(node, XPathConstants.NODESET).asInstanceOf[NodeList]
		Stream.from(0).takeWhile(_ < nodeList.getLength).map(nodeList.item)
	}

	def evalXpathNodeOpt(exe: Xpath, node: XmlNode): Option[XmlNode] =
		exe.evaluate(node, XPathConstants.NODE).asInstanceOf[Node] match {
			case null => None
			case n => Some(n)
		}

	def evalXpathNode(exe: Xpath, node: XmlNode): XmlNode =
		evalXpathNodeOpt(exe, node).getOrElse(throw new AssertionError)

	def evalXpathTextOpt(exe: Xpath, node: XmlNode): Option[String] =
		evalXpathNodeOpt(exe, node).map(xmlNodeText)

	def evalXpathText(exe: Xpath, node: XmlNode): String =
		evalXpathTextOpt(exe, node).getOrElse(throw new AssertionError)

	def dumpXml(node: XmlNode): CharSequence = {
		val tr = TransformerFactory.newInstance.newTransformer
		tr.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
		tr.setOutputProperty(OutputKeys.INDENT, "yes")
		val writer = new StringWriter
		tr.transform(new DOMSource(node), new StreamResult(writer))
		writer.getBuffer
	}
}
