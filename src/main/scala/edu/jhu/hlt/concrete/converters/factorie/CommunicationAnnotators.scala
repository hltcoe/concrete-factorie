package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token, Sentence}
import edu.jhu.hlt.concrete.Concrete.Communication
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.ner.NerLabel
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.CategoricalDomain

/**
 * @author John Sullivan
 * Document Annotators that read in a communication document (and potentially
 * some metadata annotation) and write the communication's annotation to a
 * document.
 */
abstract class CommunicationAnnotator(comm:Communication) extends DocumentAnnotator

class CommNerLabel(val token:Token, targetValue:String) extends NerLabel(targetValue) {
  def domain = new CategoricalDomain[String]
}

class CommunicationSegmenter(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = Nil

  def postAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Sentence], classOf[Token])
}

class CommunicationPOSTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence])

  def postAttrs: Iterable[Class[_]] = Seq(classOf[PTBPosLabel])
}

class CommunicationLemmatizer(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])

  def postAttrs: Iterable[Class[_]] = Seq(classOf[TokenLemma])
}

class CommunicationNERTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])

  def postAttrs: Iterable[Class[_]] = Seq(classOf[CommNerLabel])
}

class CommunicationDependencyParser(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Sentence], classOf[Token], classOf[PTBPosLabel])

  def postAttrs: Iterable[Class[_]] = Seq(classOf[ParseTree])
}
