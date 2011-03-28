#ifndef HSLIBXML_SHIM_H
#define HSLIBXML_SHIM_H

#include <libxml/parser.h>

xmlParserCtxtPtr
hslibxml_alloc_parser(const char *filename);

const char *
hslibxml_get_last_error(xmlParserCtxt *ctx);

startDocumentSAXFunc
hslibxml_getcb_start_document(xmlParserCtxt *ctx);

endDocumentSAXFunc
hslibxml_getcb_end_document(xmlParserCtxt *ctx);

startElementNsSAX2Func
hslibxml_getcb_start_element(xmlParserCtxt *ctx);

endElementNsSAX2Func
hslibxml_getcb_end_element(xmlParserCtxt *ctx);

referenceSAXFunc
hslibxml_getcb_reference(xmlParserCtxt *ctx);

charactersSAXFunc
hslibxml_getcb_characters(xmlParserCtxt *ctx);

processingInstructionSAXFunc
hslibxml_getcb_instruction(xmlParserCtxt *ctx);

commentSAXFunc
hslibxml_getcb_comment(xmlParserCtxt *ctx);

externalSubsetSAXFunc
hslibxml_getcb_external_subset(xmlParserCtxt *ctx);

void
hslibxml_setcb_start_document(xmlParserCtxt *ctx, startDocumentSAXFunc cb);

void
hslibxml_setcb_end_document(xmlParserCtxt *ctx, endDocumentSAXFunc cb);

void
hslibxml_setcb_start_element(xmlParserCtxt *ctx, startElementNsSAX2Func cb);

void
hslibxml_setcb_end_element(xmlParserCtxt *ctx, endElementNsSAX2Func cb);

void
hslibxml_setcb_reference(xmlParserCtxt *ctx, referenceSAXFunc cb);

void
hslibxml_setcb_characters(xmlParserCtxt *ctx, charactersSAXFunc cb);

void
hslibxml_setcb_instruction(xmlParserCtxt *ctx, processingInstructionSAXFunc cb);

void
hslibxml_setcb_comment(xmlParserCtxt *ctx, commentSAXFunc cb);

void
hslibxml_setcb_external_subset(xmlParserCtxt *ctx, externalSubsetSAXFunc cb);

#endif
