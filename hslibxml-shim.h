#ifndef HSLIBXML_SHIM_H
#define HSLIBXML_SHIM_H

#include <libxml/parser.h>

xmlParserCtxtPtr
hslibxml_alloc_parser(const char *filename);

const char *
hslibxml_get_last_error(xmlParserCtxt *ctx);

internalSubsetSAXFunc
hslibxml_getcb_internalSubset(xmlParserCtxt *ctx);

isStandaloneSAXFunc
hslibxml_getcb_isStandalone(xmlParserCtxt *ctx);

hasInternalSubsetSAXFunc
hslibxml_getcb_hasInternalSubset(xmlParserCtxt *ctx);

hasExternalSubsetSAXFunc
hslibxml_getcb_hasExternalSubset(xmlParserCtxt *ctx);

resolveEntitySAXFunc
hslibxml_getcb_resolveEntity(xmlParserCtxt *ctx);

getEntitySAXFunc
hslibxml_getcb_getEntity(xmlParserCtxt *ctx);

entityDeclSAXFunc
hslibxml_getcb_entityDecl(xmlParserCtxt *ctx);

notationDeclSAXFunc
hslibxml_getcb_notationDecl(xmlParserCtxt *ctx);

attributeDeclSAXFunc
hslibxml_getcb_attributeDecl(xmlParserCtxt *ctx);

elementDeclSAXFunc
hslibxml_getcb_elementDecl(xmlParserCtxt *ctx);

unparsedEntityDeclSAXFunc
hslibxml_getcb_unparsedEntityDecl(xmlParserCtxt *ctx);

startDocumentSAXFunc
hslibxml_getcb_startDocument(xmlParserCtxt *ctx);

endDocumentSAXFunc
hslibxml_getcb_endDocument(xmlParserCtxt *ctx);

referenceSAXFunc
hslibxml_getcb_reference(xmlParserCtxt *ctx);

charactersSAXFunc
hslibxml_getcb_characters(xmlParserCtxt *ctx);

ignorableWhitespaceSAXFunc
hslibxml_getcb_ignorableWhitespace(xmlParserCtxt *ctx);

processingInstructionSAXFunc
hslibxml_getcb_processingInstruction(xmlParserCtxt *ctx);

commentSAXFunc
hslibxml_getcb_comment(xmlParserCtxt *ctx);

warningSAXFunc
hslibxml_getcb_warning(xmlParserCtxt *ctx);

errorSAXFunc
hslibxml_getcb_error(xmlParserCtxt *ctx);

fatalErrorSAXFunc
hslibxml_getcb_fatalError(xmlParserCtxt *ctx);

getParameterEntitySAXFunc
hslibxml_getcb_getParameterEntity(xmlParserCtxt *ctx);

cdataBlockSAXFunc
hslibxml_getcb_cdataBlock(xmlParserCtxt *ctx);

externalSubsetSAXFunc
hslibxml_getcb_externalSubset(xmlParserCtxt *ctx);

startElementNsSAX2Func
hslibxml_getcb_startElementNs(xmlParserCtxt *ctx);

endElementNsSAX2Func
hslibxml_getcb_endElementNs(xmlParserCtxt *ctx);

xmlStructuredErrorFunc
hslibxml_getcb_serror(xmlParserCtxt *ctx);

void
hslibxml_setcb_internalSubset(xmlParserCtxt *ctx, internalSubsetSAXFunc cb);

void
hslibxml_setcb_isStandalone(xmlParserCtxt *ctx, isStandaloneSAXFunc cb);

void
hslibxml_setcb_hasInternalSubset(xmlParserCtxt *ctx, hasInternalSubsetSAXFunc cb);

void
hslibxml_setcb_hasExternalSubset(xmlParserCtxt *ctx, hasExternalSubsetSAXFunc cb);

void
hslibxml_setcb_resolveEntity(xmlParserCtxt *ctx, resolveEntitySAXFunc cb);

void
hslibxml_setcb_getEntity(xmlParserCtxt *ctx, getEntitySAXFunc cb);

void
hslibxml_setcb_entityDecl(xmlParserCtxt *ctx, entityDeclSAXFunc cb);

void
hslibxml_setcb_notationDecl(xmlParserCtxt *ctx, notationDeclSAXFunc cb);

void
hslibxml_setcb_attributeDecl(xmlParserCtxt *ctx, attributeDeclSAXFunc cb);

void
hslibxml_setcb_elementDecl(xmlParserCtxt *ctx, elementDeclSAXFunc cb);

void
hslibxml_setcb_unparsedEntityDecl(xmlParserCtxt *ctx, unparsedEntityDeclSAXFunc cb);

void
hslibxml_setcb_startDocument(xmlParserCtxt *ctx, startDocumentSAXFunc cb);

void
hslibxml_setcb_endDocument(xmlParserCtxt *ctx, endDocumentSAXFunc cb);

void
hslibxml_setcb_reference(xmlParserCtxt *ctx, referenceSAXFunc cb);

void
hslibxml_setcb_characters(xmlParserCtxt *ctx, charactersSAXFunc cb);

void
hslibxml_setcb_ignorableWhitespace(xmlParserCtxt *ctx, ignorableWhitespaceSAXFunc cb);

void
hslibxml_setcb_processingInstruction(xmlParserCtxt *ctx, processingInstructionSAXFunc cb);

void
hslibxml_setcb_comment(xmlParserCtxt *ctx, commentSAXFunc cb);

void
hslibxml_setcb_warning(xmlParserCtxt *ctx, warningSAXFunc cb);

void
hslibxml_setcb_error(xmlParserCtxt *ctx, errorSAXFunc cb);

void
hslibxml_setcb_fatalError(xmlParserCtxt *ctx, fatalErrorSAXFunc cb);

void
hslibxml_setcb_getParameterEntity(xmlParserCtxt *ctx, getParameterEntitySAXFunc cb);

void
hslibxml_setcb_cdataBlock(xmlParserCtxt *ctx, cdataBlockSAXFunc cb);

void
hslibxml_setcb_externalSubset(xmlParserCtxt *ctx, externalSubsetSAXFunc cb);

void
hslibxml_setcb_startElementNs(xmlParserCtxt *ctx, startElementNsSAX2Func cb);

void
hslibxml_setcb_endElementNs(xmlParserCtxt *ctx, endElementNsSAX2Func cb);

void
hslibxml_setcb_serror(xmlParserCtxt *ctx, xmlStructuredErrorFunc cb);

#endif
