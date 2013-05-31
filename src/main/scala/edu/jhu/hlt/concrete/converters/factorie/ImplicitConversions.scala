package edu.jhu.hlt.concrete.converters.factorie

import java.util.UUID
import edu.jhu.hlt.concrete.Concrete.{Communication, UUID => ConUUID, CommunicationGUID, TextSpan, Token => ConToken,
                                      TokenTagging, Tokenization, TokenRefSequence, Parse, Sentence => ConSentence,
                                      SentenceSegmentation, Section, SectionSegmentation}
import edu.jhu.hlt.concrete.Concrete.Parse.Constituent
import cc.factorie.app.nlp.{Sentence, Document, Token}
import scala.collection.JavaConverters._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.pos.PTBPosLabel

/**
 * @author John Sullivan
 * This object contains implicit conversions between Concrete Communications
 * and FactorIE Documents.
 */
object ImplicitConversions {

  /*
    Methods to convert Documents to Communications

   */

  private def getUUID:ConUUID = {
    val uuid = UUID.randomUUID
    ConUUID.newBuilder.setHigh(uuid.getMostSignificantBits).setLow(uuid.getLeastSignificantBits).build
  }

  implicit def Name2Guid(docName:String):CommunicationGUID = CommunicationGUID.newBuilder
    .setCorpusName(docName)
    .build

  implicit def Range2TextSpan(rng:Range):TextSpan = TextSpan.newBuilder
    .setStart(rng.start)
    .setEnd(rng.end)
    .build

  implicit def Fac2ConToken(facTok:Token):ConToken = ConToken.newBuilder
    .setTokenId(facTok.sentencePosition)
    .setText(facTok.string)
    .setTextSpan(facTok.stringStart to facTok.stringEnd)
    .build

  private def posTagging(tok:Token):Option[TokenTagging.TaggedToken] = Option(tok.posLabel) match {
    case Some(posLabel) => Some(TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(posLabel.categoryValue).build)
    case _ => None
  }

  private def nerTagging(tok:Token):Option[TokenTagging.TaggedToken] = Option(tok.nerLabel) match {
    case Some(nerLabel) => Some(TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(nerLabel.categoryValue).build)
    case _ => None
  }

  private def lemmaTagging(tok:Token):Option[TokenTagging.TaggedToken] = Option(tok.lemma) match {
    case Some(lemma) => Some(TokenTagging.TaggedToken.newBuilder.setTokenId(tok.sentencePosition).setTag(lemma.lemma).build)
    case _ => None
  }

  implicit def Tags2Tagging(toks:Iterable[Option[TokenTagging.TaggedToken]]):TokenTagging = TokenTagging.newBuilder
    .setUuid(getUUID)
    .addAllTaggedToken(toks.flatten.asJava)
    .build

  implicit def TokenIterable2Tokenization(tokens:Iterable[Token]):Tokenization = Tokenization.newBuilder
    .setUuid(getUUID)
    .setKind(Tokenization.Kind.TOKEN_LIST)
    .addAllToken((tokens map Fac2ConToken).asJava)
    .addPosTags(tokens map posTagging)
    .addNerTags(tokens map nerTagging)
    .addLemmas(tokens map lemmaTagging)
    .build

  implicit def Index2TokenRefSequence(idex:Int)(implicit uuid:ConUUID):TokenRefSequence = TokenRefSequence.newBuilder
    .addTokenId(idex)
    .setTokenization(uuid)
    .build

  //TODO Convert to Dependency Parse
  private def makeConstituent(id:Int, tree:ParseTree):Constituent = {
    val con = Constituent.newBuilder
      .setId(id)
      .setTag(tree.label(id).categoryValue)
      .addAllChildren((tree.children(id).map(_.sentencePosition) map {makeConstituent(_,tree)}).asJava)
    if(id == ParseTree.rootIndex) con.build else con.setTokenSequence(id).build
  }

  implicit def Fac2ConParse(facPars:ParseTree):Parse = Parse.newBuilder
    .setUuid(getUUID)
    .setRoot(makeConstituent(ParseTree.rootIndex, facPars))
    .build

  implicit def Fac2ConSentence(facSent:Sentence):ConSentence = ConSentence.newBuilder
    .setUuid(getUUID)
    .setTextSpan(facSent.start to facSent.end)
    .addTokenization(facSent.toSeq)
    .addParse(facSent.parse)
    .build

  implicit def FacConSentenceSegmentation(sents:Iterable[Sentence]):SentenceSegmentation = SentenceSegmentation.newBuilder
    .setUuid(getUUID)
    .addAllSentence((sents map Fac2ConSentence).asJava)
    .build

  private def Document2Communication(doc:Document):Communication = Communication.newBuilder
    .setUuid(getUUID)
    .setGuid(doc.name)
    .setText(doc.string)
    .addSectionSegmentation{
    SectionSegmentation.newBuilder // Factorie doesn't split documents into paragraphs
      .setUuid(getUUID)
      .addSection{
      Section.newBuilder
        .setUuid(getUUID)
        .setTextSpan(0 to doc.stringLength)
        .addSentenceSegmentation(doc.sentences)
        .build
    }
  }
    .build

  implicit def Document2MyDocument(doc:Document) = new MyDocument(doc)

  class MyDocument(doc:Document) {
    def asCommunication:Communication = Document2Communication(doc)
  }

  /*
    Methods to convert Communications to Documents

   */

  private def Communication2Document(comm:Communication):Document = {
    val id = comm.getGuid
    val text = comm.getText
    val document = new Document(text)
    document.setName(id.getCorpusName)

    comm.getSectionSegmentationList.asScala.foreach(ss=>{
      ss.getSectionList.asScala.foreach(sec=>{
        sec.getSentenceSegmentationList.asScala.foreach(sentSeg=>{
          sentSeg.getSentenceList.asScala.foreach(sent=>{
            if(sent.hasTextSpan){
              val sentStart = sent.getTextSpan.getStart
              val sentEnd = sent.getTextSpan.getEnd
              val sentence =
                new Sentence(document, sentStart, sentEnd-sentStart+1)

              sent.getTokenizationList.asScala.foreach(tokenization=>{
                tokenization.getTokenList.asScala.foreach(tok=>{
                  val token = new Token(sentence, tok.getText)
                  token.attr+=new TokenId(tok.getTokenId)
                })
                tokenization.getPosTagsList.asScala.foreach(tokTag=>{
                  tokTag.getTaggedTokenList.asScala.foreach(tagTok=>{
                    val tagTokId = tagTok.getTokenId
                    sentence.tokens.foreach(token=>{
                      if(token.attr[TokenId]==tagTokId){
                        token.attr+=new PTBPosLabel(token, tagTok.getTag)
                      }
                    })
                  })
                })
              })
            }
          })
        })
      })
    })
    document
  }

  implicit def Communication2MyCommunication(comm:Communication) = new MyCommunication(comm)

  class MyCommunication(comm:Communication) {
    def asDocument:Document = Communication2Document(comm)
  }
}