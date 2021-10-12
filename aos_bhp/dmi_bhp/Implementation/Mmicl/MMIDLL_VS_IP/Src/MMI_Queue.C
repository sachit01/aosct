/* queue.c
 *
 * Mats Åkerblom, Kvaser AB  1995-04-25
 * Anpassat för Esmeralda    1997-12-16
 */

/* En köhanterare. I kön lagras objekt med grundlängden elemSize.
 * När data skrivs till kön uppdateras en temporär pekare, först
 * efter anrop av queueCommit() går det senaste elementet att läsa.
 *
 * queuePut() skriver ett element, queueWrite() skriver flera.
 * Med queuePutW() kan ett element i "arbets-området" skrivas, 
 *
 * Den läsande änden kan med queueGet() läsa data.
 * queueFree() returnerar hur mycket plats det finns att skriva på,
 * queueCount() returnerar vad som finns att läsa (bägge räknar i
 * elementenheter och inte i bytes).
 */

#include <stdlib.h>
#include <memory.h>
#include "MMI_Queue.h"


/* Clears the memory of a queue; after this call, queueInit() should be called
 * but it is safe to call queueExit().
 */
void queueZap(queueT *q) {
  q->elemCount = 0;
  q->elemSize = 0;
  q->putP = q->getP = 0;
  q->buf = NULL;
}


/* Initializes and allocates the queue.
 * Returns 0 if failure (out of memory).
 */
int queueInit(queueT *q, int elemSizeI, int elemCountI) 
{
  q->elemSize = elemSizeI;
  q->elemCount = elemCountI;
  q->putP = q->getP = 0;
  
  q->buf = (byte*)malloc((uint)(elemSizeI * elemCountI));
  if (q->buf == NULL) {
    q->elemCount = 0;
    return 0;
  } 
  else {
    return 1;
  }
}

/* Initializes and allocates the queue.
 * Returns 0 if failure (out of memory).
 */
void queueExit(queueT *q) 
{
  if (q->buf) {
    free(q->buf);
    q->buf = NULL;
  } 
}

/* Writes an element to the queues working set.
 */
void *queuePutW(queueT *q) 
{
  if (queueFree(q) < 1) {
    return NULL;
  }
  return (q->buf + (q->putP * q->elemSize));
}

/* Writes an element to the queue.
 * Returns 0 if there was no room 
 * Identical to a queuePutW() followed by a queueCommit().
 */
int queuePut(queueT *q, void *e) 
{
  void *p = queuePutW(q);
  if (!p)
    return 0;
  memcpy(p, e, (uint)q->elemSize);
  queueCommit(q);
  return 1;
}

/* Makes the current working set accessible.
 */
void queueCommit(queueT *q) 
{
  if (++q->putP >= q->elemCount)
    q->putP = 0;
}

/* Discards the next message
 */
int queueDrop(queueT *q) 
{
  if (q->getP == q->putP)
    return 0; /* queue is empty */
  if (++q->getP >= q->elemCount)
    q->getP = 0;
  return 1;
}


/* Returns a pointer to the next element that get will return or NULL if the
 * queue is empty.
 */
void *queueGetW(queueT *q) {
  if (q->getP == q->putP)
    return NULL;
  else
    return q->buf + (q->getP * q->elemSize);
}

/* Gets an element from the queue; returns 0 if it was empty.
 */
int queueGet(queueT *q, void *e) {
  void *p = queueGetW(q);
  if (p == NULL)
    return 0;
  memcpy(e, p, (uint)q->elemSize);
  if (queueDrop(q) == 1)
    return 1;
  else 
    return 0;
}

/* The number of elements in the queue that can be read.
 * getP or putP might get changed when we do the calculation, but the number
 * returned will at least have been valid very recently.
 */
int queueCount(queueT *q) {
  int c = q->putP - q->getP;
  if (c < 0) {
    c = c + q->elemCount;
  }
  return c;
}

/* Returns how many elements can be written to the queue right now.
 */
int queueFree(queueT *q) {
  int c = q->putP - q->getP;
  if (c < 0) {
    c = c + q->elemCount;
  }
  return q->elemCount-c-1;
}

