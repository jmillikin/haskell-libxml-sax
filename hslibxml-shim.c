#include "hslibxml-shim.h"
#include <string.h>

xmlParserCtxtPtr
hslibxml_alloc_parser(const char *filename)
{
	xmlSAXHandler sax;
	xmlParserCtxt *ctx;
	
	memset(&sax, 0, sizeof(xmlSAXHandler));
	sax.initialized = XML_SAX2_MAGIC;
	ctx = xmlCreatePushParserCtxt(&sax, NULL, NULL, 0, filename);
	ctx->replaceEntities = 1;
	return ctx;
}

const char *
hslibxml_get_last_error(xmlParserCtxt *ctx)
{
	return xmlCtxtGetLastError(ctx)->message;
}

int
hslibxml_want_callback(xmlParserCtxt *ctx, void *cb_ctx)
{
	if (ctx->replaceEntities)
	{
		return 1;
	}
	
	return (ctx == cb_ctx);
}

internalSubsetSAXFunc
hslibxml_getcb_internalSubset(xmlParserCtxt *ctx)
{
	return ctx->sax->internalSubset;
}

isStandaloneSAXFunc
hslibxml_getcb_isStandalone(xmlParserCtxt *ctx)
{
	return ctx->sax->isStandalone;
}

hasInternalSubsetSAXFunc
hslibxml_getcb_hasInternalSubset(xmlParserCtxt *ctx)
{
	return ctx->sax->hasInternalSubset;
}

hasExternalSubsetSAXFunc
hslibxml_getcb_hasExternalSubset(xmlParserCtxt *ctx)
{
	return ctx->sax->hasExternalSubset;
}

resolveEntitySAXFunc
hslibxml_getcb_resolveEntity(xmlParserCtxt *ctx)
{
	return ctx->sax->resolveEntity;
}

getEntitySAXFunc
hslibxml_getcb_getEntity(xmlParserCtxt *ctx)
{
	return ctx->sax->getEntity;
}

entityDeclSAXFunc
hslibxml_getcb_entityDecl(xmlParserCtxt *ctx)
{
	return ctx->sax->entityDecl;
}

notationDeclSAXFunc
hslibxml_getcb_notationDecl(xmlParserCtxt *ctx)
{
	return ctx->sax->notationDecl;
}

attributeDeclSAXFunc
hslibxml_getcb_attributeDecl(xmlParserCtxt *ctx)
{
	return ctx->sax->attributeDecl;
}

elementDeclSAXFunc
hslibxml_getcb_elementDecl(xmlParserCtxt *ctx)
{
	return ctx->sax->elementDecl;
}

unparsedEntityDeclSAXFunc
hslibxml_getcb_unparsedEntityDecl(xmlParserCtxt *ctx)
{
	return ctx->sax->unparsedEntityDecl;
}

startDocumentSAXFunc
hslibxml_getcb_startDocument(xmlParserCtxt *ctx)
{
	return ctx->sax->startDocument;
}

endDocumentSAXFunc
hslibxml_getcb_endDocument(xmlParserCtxt *ctx)
{
	return ctx->sax->endDocument;
}

referenceSAXFunc
hslibxml_getcb_reference(xmlParserCtxt *ctx)
{
	return ctx->sax->reference;
}

charactersSAXFunc
hslibxml_getcb_characters(xmlParserCtxt *ctx)
{
	return ctx->sax->characters;
}

ignorableWhitespaceSAXFunc
hslibxml_getcb_ignorableWhitespace(xmlParserCtxt *ctx)
{
	return ctx->sax->ignorableWhitespace;
}

processingInstructionSAXFunc
hslibxml_getcb_processingInstruction(xmlParserCtxt *ctx)
{
	return ctx->sax->processingInstruction;
}

commentSAXFunc
hslibxml_getcb_comment(xmlParserCtxt *ctx)
{
	return ctx->sax->comment;
}

warningSAXFunc
hslibxml_getcb_warning(xmlParserCtxt *ctx)
{
	return ctx->sax->warning;
}

errorSAXFunc
hslibxml_getcb_error(xmlParserCtxt *ctx)
{
	return ctx->sax->error;
}

fatalErrorSAXFunc
hslibxml_getcb_fatalError(xmlParserCtxt *ctx)
{
	return ctx->sax->fatalError;
}

getParameterEntitySAXFunc
hslibxml_getcb_getParameterEntity(xmlParserCtxt *ctx)
{
	return ctx->sax->getParameterEntity;
}

cdataBlockSAXFunc
hslibxml_getcb_cdataBlock(xmlParserCtxt *ctx)
{
	return ctx->sax->cdataBlock;
}

externalSubsetSAXFunc
hslibxml_getcb_externalSubset(xmlParserCtxt *ctx)
{
	return ctx->sax->externalSubset;
}

startElementNsSAX2Func
hslibxml_getcb_startElementNs(xmlParserCtxt *ctx)
{
	return ctx->sax->startElementNs;
}

endElementNsSAX2Func
hslibxml_getcb_endElementNs(xmlParserCtxt *ctx)
{
	return ctx->sax->endElementNs;
}

xmlStructuredErrorFunc
hslibxml_getcb_serror(xmlParserCtxt *ctx)
{
	return ctx->sax->serror;
}

void
hslibxml_setcb_internalSubset(xmlParserCtxt *ctx, internalSubsetSAXFunc cb)
{
	ctx->sax->internalSubset = cb;
}

void
hslibxml_setcb_isStandalone(xmlParserCtxt *ctx, isStandaloneSAXFunc cb)
{
	ctx->sax->isStandalone = cb;
}

void
hslibxml_setcb_hasInternalSubset(xmlParserCtxt *ctx, hasInternalSubsetSAXFunc cb)
{
	ctx->sax->hasInternalSubset = cb;
}

void
hslibxml_setcb_hasExternalSubset(xmlParserCtxt *ctx, hasExternalSubsetSAXFunc cb)
{
	ctx->sax->hasExternalSubset = cb;
}

void
hslibxml_setcb_resolveEntity(xmlParserCtxt *ctx, resolveEntitySAXFunc cb)
{
	ctx->sax->resolveEntity = cb;
}

void
hslibxml_setcb_getEntity(xmlParserCtxt *ctx, getEntitySAXFunc cb)
{
	ctx->sax->getEntity = cb;
}

void
hslibxml_setcb_entityDecl(xmlParserCtxt *ctx, entityDeclSAXFunc cb)
{
	ctx->sax->entityDecl = cb;
}

void
hslibxml_setcb_notationDecl(xmlParserCtxt *ctx, notationDeclSAXFunc cb)
{
	ctx->sax->notationDecl = cb;
}

void
hslibxml_setcb_attributeDecl(xmlParserCtxt *ctx, attributeDeclSAXFunc cb)
{
	ctx->sax->attributeDecl = cb;
}

void
hslibxml_setcb_elementDecl(xmlParserCtxt *ctx, elementDeclSAXFunc cb)
{
	ctx->sax->elementDecl = cb;
}

void
hslibxml_setcb_unparsedEntityDecl(xmlParserCtxt *ctx, unparsedEntityDeclSAXFunc cb)
{
	ctx->sax->unparsedEntityDecl = cb;
}

void
hslibxml_setcb_startDocument(xmlParserCtxt *ctx, startDocumentSAXFunc cb)
{
	ctx->sax->startDocument = cb;
}

void
hslibxml_setcb_endDocument(xmlParserCtxt *ctx, endDocumentSAXFunc cb)
{
	ctx->sax->endDocument = cb;
}

void
hslibxml_setcb_reference(xmlParserCtxt *ctx, referenceSAXFunc cb)
{
	ctx->sax->reference = cb;
	
	if (cb == NULL)
	{ ctx->replaceEntities = 1; }
	
	else
	{ ctx->replaceEntities = 0; }
}

void
hslibxml_setcb_characters(xmlParserCtxt *ctx, charactersSAXFunc cb)
{
	ctx->sax->characters = cb;
}

void
hslibxml_setcb_ignorableWhitespace(xmlParserCtxt *ctx, ignorableWhitespaceSAXFunc cb)
{
	ctx->sax->ignorableWhitespace = cb;
}

void
hslibxml_setcb_processingInstruction(xmlParserCtxt *ctx, processingInstructionSAXFunc cb)
{
	ctx->sax->processingInstruction = cb;
}

void
hslibxml_setcb_comment(xmlParserCtxt *ctx, commentSAXFunc cb)
{
	ctx->sax->comment = cb;
}

void
hslibxml_setcb_warning(xmlParserCtxt *ctx, warningSAXFunc cb)
{
	ctx->sax->warning = cb;
}

void
hslibxml_setcb_error(xmlParserCtxt *ctx, errorSAXFunc cb)
{
	ctx->sax->error = cb;
}

void
hslibxml_setcb_fatalError(xmlParserCtxt *ctx, fatalErrorSAXFunc cb)
{
	ctx->sax->fatalError = cb;
}

void
hslibxml_setcb_getParameterEntity(xmlParserCtxt *ctx, getParameterEntitySAXFunc cb)
{
	ctx->sax->getParameterEntity = cb;
}

void
hslibxml_setcb_cdataBlock(xmlParserCtxt *ctx, cdataBlockSAXFunc cb)
{
	ctx->sax->cdataBlock = cb;
}

void
hslibxml_setcb_externalSubset(xmlParserCtxt *ctx, externalSubsetSAXFunc cb)
{
	ctx->sax->externalSubset = cb;
}

void
hslibxml_setcb_startElementNs(xmlParserCtxt *ctx, startElementNsSAX2Func cb)
{
	ctx->sax->startElementNs = cb;
}

void
hslibxml_setcb_endElementNs(xmlParserCtxt *ctx, endElementNsSAX2Func cb)
{
	ctx->sax->endElementNs = cb;
}

void
hslibxml_setcb_serror(xmlParserCtxt *ctx, xmlStructuredErrorFunc cb)
{
	ctx->sax->serror = cb;
}
