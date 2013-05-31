package edu.jhu.hlt.concrete.converters.factorie

import java.util.UUID
import edu.jhu.hlt.concrete.Concrete
import cc.factorie.app.nlp.{Sentence, Document, Token}
import scala.collection.JavaConverters._
import cc.factorie.app.nlp.parse.ParseTree

/**
 * @author John Sullivan
 * This object contains implicit conversions between Concrete Communications
 * and FactorIE Documents.
 */
object ImplicitConversions {

  private def getUUID:Concrete.UUID = {
    val uuid = UUID.randomUUID
    Concrete.UUID.newBuilder.setHigh(uuid.getMostSignificantBits).setLow(uuid.getLeastSignificantBits).build
  }

  implicit def Name2Guid(docName:String):Concrete.CommunicationGUID = Concrete.CommunicationGUID.newBuilder
    .setCorpusName(docName)
    .build

  implicit def Range2TextSpan(rng:Range):Concrete.TextSpan = Concrete.TextSpan.newBuilder
    .setStart(rng.start)
    .setEnd(rng.end)
    .build

  implicit def Fac2ConToken(facTok:Token):Concrete.Token = Concrete.Token.newBuilder
    .setTokenId(facTok.sentencePosition)
    .setText(facTok.string)
    .setTextSpan(facTok.stringStart to facTok.stringEnd)
    .build

  private def posTagging(tok:Token):Option[Concrete.TokenTagging.TaggedToken] = Option(tok.posLabel) match {
    case Some(posLabel) => Some(Concrete.TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(posLabel.categoryValue).build)
    case _ => None
  }

  private def nerTagging(tok:Token):Option[Concrete.TokenTagging.TaggedToken] = Option(tok.nerLabel) match {
    case Some(nerLabel) => Some(Concrete.TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(nerLabel.categoryValue).build)
    case _ => None
  }

  private def lemmaTagging(tok:Token):Option[Concrete.TokenTagging.TaggedToken] = Option(tok.lemma) match {
    case Some(lemma) => Some(Concrete.TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(lemma.lemma).build)
    case _ => None
  }

  implicit def Tags2Tagging(toks:Iterable[Option[Concrete.TokenTagging.TaggedToken]]):Concrete.TokenTagging = Concrete.TokenTagging.newBuilder
    .setUuid(getUUID)
    .addAllTaggedToken(toks.flatten.asJava)
    .build

  implicit def TokenIterable2Tokenization(tokens:Iterable[Token]):Concrete.Tokenization = Concrete.Tokenization.newBuilder
    .setUuid(getUUID)
    .setKind(Concrete.Tokenization.Kind.TOKEN_LIST)
    .addAllToken((tokens map Fac2ConToken).asJava)
    .addPosTags(tokens map posTagging)
    .addNerTags(tokens map nerTagging)
    .addLemmas(tokens map lemmaTagging)
    .build

  implicit def Index2TokenRefSequence(idex:Int)(implicit uuid:Concrete.UUID):Concrete.TokenRefSequence = Concrete.TokenRefSequence.newBuilder
    .addTokenId(idex)
    .setTokenization(uuid)
    .build

  //TODO Convert to Dependency Parse
  private def makeConstituent(id:Int, tree:ParseTree):Concrete.Parse.Constituent = {
    val con = Concrete.Parse.Constituent.newBuilder
      .setId(id)
      .setTag(tree.label(id).categoryValue)
      .addAllChildren((tree.children(id).map(_.sentencePosition) map {makeConstituent(_,tree)}).asJava)
    if(id == ParseTree.rootIndex) con.build else con.setTokenSequence(id).build
  }

  implicit def Fac2ConParse(facPars:ParseTree):Concrete.Parse = Concrete.Parse.newBuilder
    .setUuid(getUUID)
    .setRoot(makeConstituent(ParseTree.rootIndex, facPars))
    .build

  implicit def Fac2ConSentence(facSent:Sentence):Concrete.Sentence = Concrete.Sentence.newBuilder
    .setUuid(getUUID)
    .setTextSpan(facSent.start to facSent.end)
    .addTokenization(facSent.toSeq)
    .addParse(facSent.parse)
    .build

  implicit def FacConSentenceSegmentation(sents:Iterable[Sentence]):Concrete.SentenceSegmentation = Concrete.SentenceSegmentation.newBuilder
    .setUuid(getUUID)
    .addAllSentence((sents map Fac2ConSentence).asJava)
    .build

  implicit def Document2Communication(doc:Document):Concrete.Communication = Concrete.Communication.newBuilder
    .setUuid(getUUID)
    .setGuid(doc.name)
    .setText(doc.string)
    .addSectionSegmentation{
    Concrete.SectionSegmentation.newBuilder // Factorie doesn't split documents into paragraphs
      .setUuid(getUUID)
      .addSection{
      Concrete.Section.newBuilder
        .setUuid(getUUID)
        .setTextSpan(0 to doc.stringLength)
        .addSentenceSegmentation(doc.sentences)
        .build
    }
  }
    .build

  implicit def Document2MyDocument(doc:Document) = new MyDocument(doc)

  class MyDocument(doc:Document) {
    def asCommunication:Concrete.Communication = Document2Communication(doc)
  }
}