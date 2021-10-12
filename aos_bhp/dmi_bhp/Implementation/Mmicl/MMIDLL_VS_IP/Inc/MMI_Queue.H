/* iqueue.h
 *
 * Mats èkerblom, Kvaser AB  1995-04-25
 * Last changed              1995-04-25
 * Tailored for BHP          2017-08-22 Bo Hermansson
 */

#ifndef __QUEUE_H
#define __QUEUE_H
#include "mmi_types.h"

typedef struct {
  int getP, putP, putPw;
  byte *buf;
  int elemSize;
  int elemCount;
} queueT;


void queueZap(queueT *q);
int queueInit(queueT *q, int elemSizeI, int elemCountI);
void queueExit(queueT *q);
void *queuePutW(queueT *q);
int queuePut(queueT *q, void *e);
void queueCommit(queueT *q);
int queueGet(queueT *q, void *e);
int queueCount(queueT *q);
int queueFree(queueT *q);
int queueDrop(queueT *q);
void *queueGetW(queueT *q);

#endif
