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
	return ctx;
}

const char *
hslibxml_get_last_error(xmlParserCtxt *ctx)
{
	return xmlCtxtGetLastError(ctx)->message;
}

startDocumentSAXFunc
hslibxml_getcb_start_document(xmlParserCtxt *ctx)
{
	return ctx->sax->startDocument;
}

endDocumentSAXFunc
hslibxml_getcb_end_document(xmlParserCtxt *ctx)
{
	return ctx->sax->endDocument;
}

startElementNsSAX2Func
hslibxml_getcb_start_element(xmlParserCtxt *ctx)
{
	return ctx->sax->startElementNs;
}

endElementNsSAX2Func
hslibxml_getcb_end_element(xmlParserCtxt *ctx)
{
	return ctx->sax->endElementNs;
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

processingInstructionSAXFunc
hslibxml_getcb_instruction(xmlParserCtxt *ctx)
{
	return ctx->sax->processingInstruction;
}

commentSAXFunc
hslibxml_getcb_comment(xmlParserCtxt *ctx)
{
	return ctx->sax->comment;
}

externalSubsetSAXFunc
hslibxml_getcb_external_subset(xmlParserCtxt *ctx)
{
	return ctx->sax->externalSubset;
}

void
hslibxml_setcb_start_document(xmlParserCtxt *ctx, startDocumentSAXFunc cb)
{
	ctx->sax->startDocument = cb;
}

void
hslibxml_setcb_end_document(xmlParserCtxt *ctx, endDocumentSAXFunc cb)
{
	ctx->sax->endDocument = cb;
}

void
hslibxml_setcb_start_element(xmlParserCtxt *ctx, startElementNsSAX2Func cb)
{
	ctx->sax->startElementNs = cb;
}

void
hslibxml_setcb_end_element(xmlParserCtxt *ctx, endElementNsSAX2Func cb)
{
	ctx->sax->endElementNs = cb;
}

void
hslibxml_setcb_reference(xmlParserCtxt *ctx, referenceSAXFunc cb)
{
	ctx->sax->reference = cb;
}

void
hslibxml_setcb_characters(xmlParserCtxt *ctx, charactersSAXFunc cb)
{
	ctx->sax->characters = cb;
}

void
hslibxml_setcb_instruction(xmlParserCtxt *ctx, processingInstructionSAXFunc cb)
{
	ctx->sax->processingInstruction = cb;
}

void
hslibxml_setcb_comment(xmlParserCtxt *ctx, commentSAXFunc cb)
{
	ctx->sax->comment = cb;
}

void
hslibxml_setcb_external_subset(xmlParserCtxt *ctx, externalSubsetSAXFunc cb)
{
	ctx->sax->externalSubset = cb;
}
