/**
\if AsMainPage
\mainpage Dispatcher A- and B-Channel
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-04-04 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction

\subsection Design Design Overview

To verify that the correct data-message is transmitted from the AOS to external systems, the first half of the message is taken from ATP-A and the second half from ATP-B.
The two parts is merged in the dispatcher and sent further to the proper destination.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
-          | -                                     | -

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

@image html atpa_atpb_dispatcher.png
@image latex atpa_atpb_dispatcher.png

\section DetailedDescriptionOfComponents Detailed Description of Components

The affected components is located in both ATP and Dispatcher. The ATP CrossCompare component is using vfw-channels to transmit messages to the Dispatcher. This needs to be modified to distinguish between ATP-A and and ATP-B and send only half message
each further to the Dispatcher.

In the Dispatcher, the execution is asynchronous (callback methods when something is received on any vfw-channel). The current implementation is only processing messages from the ATP-A, and needs to be modified to:
- Receive and store message
- Check if message has been received from other ATP
- If received -> Merge and transmit message
- If not received -> Exit and wait for next message

@image html dispatcher_sequence.png
@image latex dispatcher_sequence.png

\subsection ATP ATP

\subsubsection CrossCompare Cross Compare

<br><code>
void ATP::Support::CrossCompareOutputChannel::commit()
</code>

Distinguish between A and B, and send only first half of 'buffer' from A, and second half from B.
If uneven length, send 1 extra byte from A.


\subsection Dispatcher Dispatcher

\subsubsection TypesAndStorage Types and Storage

A new queue entry type is needed to store incoming messages from ATP-A and ATP-B:

<br>
\code{.c}
struct
{
VFW_Buffer inputVfwBuffer;
uint8_t inputBuffer[maxInputMessageSize]
uint8_t connId;
} OutgoingDispMsg;
\endcode

The following items can be removed from current 'struct ConnectionItem':
- vfwTempSize
- inputBuffer (moved to queue entry)
- inputVfwBuffer (moved to queue entry)


2 new queues for outgoing messages coming from ATPA/B:

<br>
\code{.c}
typedef ATC::GPList<OutgoingDispMsg, 5, NULL> OutgoingDispMsgQueue;

OutgoingDispMsgQueue OutgoingDispMsgQueueATPA;
OutgoingDispMsgQueue OutgoingDispMsgQueueATPB;
\endcode

\subsubsection Initialization Initialization

<br><code>
void MessageDispatcher::readChannelInit(void)
</code>
- Initialize B_SIDE as well.
- Set prevRecMsgSide parameter to A_SIDE or B_SIDE.
- Initialize Supervision Timer 

\subsubsection Processing Processing

<br><code>
void MessageDispatcher::handleMessageFromATP(VFW_SyncChannel channel, void *data)
</code>

- Split method to only read message from ATP A/B and put in queue for respective A- or B-Side (OutgoingDispMsgQueueATPA/OutgoingDispMsgQueueATPB).

<br><code>
void pollQueuesFromATP()
</code>

- Check in A- and B-queue for message. If both has message -> Merge messages
- If Message received from only one queue -> Start Timer to supervise the reception of the other half.
- Re-use implementation from processing part of handleMessageFromATP() and handleMessageToLigFromATP and make 2 new methods:
MergeAndSendLigMessage() and MergeAndSendMessage()
- If DataType == LigData then MergeAndSendLigMessage() else MergeAndSendMessage()

\subsubsection Supervising Supervising

<br><code>
void runIn()
</code>

- Use the vfwTimerCheck() to supervise that a message is taken care of within a certain time.(100ms max)  
- Issue an event if timer is exceeded.

\section ImplementationDistribution Task Distribution

Implementation is done in one task
 
\section AdditionalMaterial Additional Material

*/