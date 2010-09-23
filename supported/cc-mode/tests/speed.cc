int foo()
{}


/***************************************************************************
	Copyright (C) Trillium A Division of LTX 1990. All rights reserved.

	Module:
		Flow.cxx

	Created:
		6/90		Michael D. Carney

	Notes:
	TR_DEBUG Flags for FLOW_SUPPORT should be used as follows:
                         
	BITS			USAGE
	================================================================
	0-3		used for the base class BaseNode.
	4-7		used for the class SpecPair
	8-11		used for the class FlowNode
	12-15		used for the class PortNumber
	16-19		used for the class Bin
	20-23		used for the class StartNode
	24-27		used for the class PortList
	28-31		used for the 4gl symbol class Flow
	
	NOTE:
	In each case, the following paradigm was used
	Lowest bit	used for entries and exits on constructors and destructors
	Low middle bit	used for internals on constructors and destructors
	High middle bit	used for entries and exits on methods
	High bit	used for internals on methods

	History:
		6/90		MDC		Created

 ***************************************************************************/

#include <memory.h>
#include <limits.h>
#include <Flow.hxx>
#include <Spec.hxx>
#include <EV_Buf.hxx>
#include <TimeStamp.hxx>
#include <utility_macros.h>
#include <EVResource.hxx>

static char *SCCS = "@(#) 6/6/94 8.26 Flow.cxx LTX";


#define STANDARD_CALIBRATION_EXPR "StdCal"

/***************************************************************************
			class ev_ExitPortArray
 ***************************************************************************/

ev_ExitPortArray::
ev_ExitPortArray(const ev_ExitPortArray &array, Flow *ptr)
    : Shmptr1dArray(), ParentFlow(*ptr) {
    int size = array.GetSize();
    for (int ii = size-1; ii>= 0; ii--)
        {
            if (array[ii] != SHMNULL)
                (*this)[ii] = *new ev_ExitPort(*array[ii].dref(),ptr);
        }
}

ev_ExitPort* ev_ExitPortArray::
ObtainExitPort(int exit_port) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    if ( operator[](exit_port) == SHMNULL)
        {
            ev_ExitPort *ptr = new ev_ExitPort( ParentFlow.dref() );
            operator[](exit_port) = *ptr;
        }
    return operator[](exit_port).dref();
};		/* AddExitPort() */

ev_ExitPortArray::
~ev_ExitPortArray() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    for(int index=0; index < GetSize(); index++)
        {
            ev_ExitPort*ptr = operator[](index).dref();
            delete ptr;
            operator[](index) = SHMNULL;
        }
};


/***************************************************************************
			class ev_ExitPort
 ***************************************************************************/

ev_ExitPort::
ev_ExitPort(Flow *flow)
    : BaseSharedObject(),
      ParentFlow(*flow),
      NextNode(), 
      ActionExpression(),
      IntervalTime(),
      LocalRunNumber(0), 
      TraverseCount(0),
      CumulativeTraverseCount(0) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    IntervalTime.JamType("S");
    IntervalTime.SetOutputExpr();
    IntervalTime.Change("#");
    IntervalTime.Jam(0.0);
    IntervalTime.SetPersistance(PERSIST_DEVICE);
};

ev_ExitPort::
ev_ExitPort(const ev_ExitPort &port, Flow *flow)
// Connections of the next node handled by the parent
// flow object 
    : BaseSharedObject(),
      ParentFlow(*flow),
      NextNode(), 
      ActionExpression(port.ActionExpression,*flow,__FILE__, __LINE__),
      IntervalTime(port.IntervalTime,*flow,__FILE__, __LINE__),
      LocalRunNumber(0),
      TraverseCount(0),
      CumulativeTraverseCount(0) {
    ActionExpression.SetBasePointer(*flow);
    IntervalTime.SetBasePointer(*flow);
}

ev_ExitPort::
~ev_ExitPort() {
}

boolean ev_ExitPort::
PutNode(ev_BaseNode *node) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    boolean rval = FALSE;

    if (NextNode.dref () != NULL)
	rval = FALSE;
    else
        {
            NextNode = *node;
            rval = TRUE;
        }

    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  ev_ExitPort::PutNode()=%d, this=%s",
	      rval, Id());
    return rval;
};

void  ev_ExitPort::
CutNode() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    NextNode = SHMNULL;
};

void  ev_ExitPort::
IncTraverseCount() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int run_num = GetFlowsRunNumber();
    if (GetLocalRunNumber() < run_num )
        {
            SetLocalRunNumber( run_num );
            SetTraverseCount(0);
        }
    TraverseCount++;
    CumulativeTraverseCount++;
};

int ev_ExitPort::
GetFlowsRunNumber() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    return ParentFlow -> GetRunNumber();
};


void ev_ExitPort::
DebugDump (FILE * file, int indent) {

    fprintf (file, "%sExitPort this=%08x:\n", Indent(indent), this);
    if (this)
        {
            fprintf (file,
                     "%sParentFlow = %s\n",
                     Indent(indent+1), ParentFlow -> GetFullName ());
            fprintf (file,
                     "%sNextNode = %s\n",
                     Indent(indent+1), NextNode->Id());
            fprintf (file,
                     "%sActionExpression = %s\n",
                     Indent(indent+1), ActionExpression.Id());
            fprintf (file,
                     "%sIntervalTime = %s\n",
                     Indent(indent+1), IntervalTime.Id());
            fprintf (file,
                     "%sLocalRunNumber=%d\t(FlowRunNumber=%d)"
                     "\tCount = %d\tCumulativeTraverseCount=%d\n",
                     Indent(indent+1), LocalRunNumber,
                     GetFlowsRunNumber(), TraverseCount, CumulativeTraverseCount );
        }
}		/* DebugDump() */


/***************************************************************************
			class ev_BaseNode
 ***************************************************************************/


ev_BaseNode::
ev_BaseNode (node_type type, Flow *parent, int index, int max_exit_ports)
    : BaseSharedObject(),
      ParentFlow(*parent),
      Tag(),
      Index(index),
      Type(type),
      MaxNumExitPorts(max_exit_ports),
      portX(-1),
      portY(-1),
      ParentNode(),
      ExitPorts(parent),
      ExitedPort(-1),
      InternalTimer(),
      InternalTime(),
      CumulativeTimer(),
      CumulativeTime() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (parent != NULL);
    tr_assert (parent -> GetType () == EV_FLOW);
    tr_assert (index >= 0);


    InternalTime.JamType("S");
    InternalTime.SetOutputExpr();
    InternalTime.Change("#");
    InternalTime.Jam(0.0);
    InternalTime.SetPersistance(PERSIST_DEVICE);

    CumulativeTime.JamType("S");
    CumulativeTime.SetOutputExpr();
    CumulativeTime.Change("#");
    CumulativeTime.Jam(0.0);

    TR_DEBUG1(FLOW_SUPPORT, BIT_0, FALSE,
	      "EXIT  ev_BaseNode::ev_BaseNode(), this=%s",
	      Id ());
}

ev_BaseNode::
ev_BaseNode(const ev_BaseNode &node, Flow *parent) :
    BaseSharedObject(), ParentFlow(*parent), Tag(node.Tag),
    Index(node.Index), Type(node.Type), portX(node.portX),
    portY(node.portY), ParentNode(),
    ExitPorts(parent), ExitedPort(-1),
    InternalTimer(), InternalTime(node.InternalTime,*parent,__FILE__,__LINE__),
    CumulativeTimer(), CumulativeTime(node.CumulativeTime,*parent,__FILE__,__LINE__),
    MaxNumExitPorts(node.MaxNumExitPorts) {
}

ev_BaseNode::
~ev_BaseNode () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG1 (FLOW_SUPPORT, BIT_0, FALSE, "EXIT  ev_BaseNode::~ev_BaseNode(), this = %x,%s", Id ());
}

const char *ev_BaseNode::
Id() {
    if (this)
        {
            char id_buf[128];
	
            const char *type_str;
            switch(GetType())
                {
                case NODE_FLOW:
                    type_str = "FlowNode";
                    break;
                case NODE_NUMBER:
                    type_str = "PortNumber";
                    break;
                case NODE_BIN:
                    type_str = "Bin";
                    break;
                case NODE_START:
                    type_str = "StartNode";
                    break;
                default:
                    type_str = "Unknown";
                    break;
                }

            sprintf(id_buf,
                    "%s (%d,%d) at %s",
                    type_str,portX,portY,BaseSharedObject::Id());
            return EV_Buffer::Submit(id_buf);
        }
    else
	return "BaseNode:this=NULL";
}

int ev_BaseNode::
GetFlowsRunNumber() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    return ParentFlow->GetRunNumber();
}

void ev_BaseNode::
Connect (ev_BaseNode * p) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (p != NULL);
    int size = ParentNode.GetSize ();

    TR_DEBUG1 (FLOW_SUPPORT, BIT_3, FALSE,
	       "Sizeof ParentNode array = %d",
	       size);

    int i = 0;
    while (p <= ParentNode[i].dref ())
	i++;

    TR_DEBUG3 (FLOW_SUPPORT, BIT_3, FALSE,
	       "p = %x, ParentNode[%d].dref() = %x\n",
	       p, i, ParentNode[i].dref ());

    if (p != ParentNode[i].dref ())
        {
            if (TR_DEBUG (FLOW_SUPPORT, BIT_3, FALSE))
                {
                    fprintf (stderr, "Inserting new Parent node at index %d", i);
                    fprintf (stderr, "ParentNode array BEFORE insertion:\n");
                    ParentNode.PrintIt ();
                }

                /* make room for the new ParentNode */
            ParentNode.Skootch (i);
                /* Add it in */
            ParentNode[i] = *p;
            EV_CURTP->ResetChangeFlags((EVG_PATTERN_EVALUATED | EVG_GLOBAL_WID_UTD));

            if (TR_DEBUG (FLOW_SUPPORT, BIT_3, FALSE))
                {
                    fprintf (stderr, "Done Inserting new Parent node at index %d", i);
                    fprintf (stderr, "ParentNode array AFTER insertion:\n");
                    ParentNode.PrintIt ();
                }
        }
    TR_DEBUG2 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode::Connect(p = %s)), this = %s",
	       p->Id(), Id());
}

void ev_BaseNode::
Disconnect (ev_BaseNode * p) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (p != NULL);
    int size = ParentNode.GetSize ();

    TR_DEBUG1 (FLOW_SUPPORT, BIT_3, FALSE,
	       "Sizeof ParentNode array = %d",
	       size);

    int i = 0;
    while ((i < size) && (p != ParentNode[i].dref ()))
	i++;

    TR_DEBUG3 (FLOW_SUPPORT, BIT_3, FALSE,
	       "p = %x, ParentNode[%d].dref() = %x\n",
	       p, i, ParentNode[i].dref ());

    if (i < size)
        {
            if (TR_DEBUG (FLOW_SUPPORT, 3, FALSE))
                {
                    fprintf (stderr, "Deleting Parent node at index %d", i);
                    fprintf (stderr, "ParentNode array BEFORE deletion:\n");
                    ParentNode.PrintIt ();
                }

                /* SHMNULL out the ith entry */
            ParentNode[i] = SHMNULL;
                /* now squeeze it out */
            ParentNode.Squeeze (i);
            EV_CURTP->ResetChangeFlags((EVG_PATTERN_EVALUATED | EVG_GLOBAL_WID_UTD));

            if (TR_DEBUG (FLOW_SUPPORT, BIT_3, FALSE))
                {
                    fprintf (stderr, "Done Deleting Parent node at index %d", i);
                    fprintf (stderr, "ParentNode array AFTER deletion:\n");
                    ParentNode.PrintIt ();
                }
        }
    TR_DEBUG2 (FLOW_SUPPORT, BIT_2, FALSE, "EXIT  ev_BaseNode::Disconnect(p = %s), this = %s", p->Id(), Id());
}

int ev_BaseNode::
NumOfChildren () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval;
    int num = ExitPorts.LargestDefined () + 1;

    rval = (num > RequestedPorts ? num : RequestedPorts);
    TR_DEBUG4 (FLOW_SUPPORT, BIT_10, FALSE, 
	       "EXIT  ev_BaseNode::NumOfChildren()=%d, num=%d, RequestedPorts=%d, this=%s", 
	       rval, num, RequestedPorts, Id() );
    return rval;
}

boolean ev_BaseNode::
AnyChildren () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    boolean rval;

    if (NumOfChildren () > 0)
	rval = TRUE;
    else
	rval = FALSE;
    TR_DEBUG2 (FLOW_SUPPORT, BIT_10, FALSE, "EXIT  ev_BaseNode::AnyChildren()=%d, this=%s", rval, Id());
    return rval;
}

void ev_BaseNode::
DefinePorts (int upto) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (upto >= 0);
    RequestedPorts = upto;
    TR_DEBUG3 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  ev_BaseNode::DefinePorts(%d) got %d, this=%s",
	       upto, RequestedPorts, Id());
}

void ev_BaseNode::
Move (int nx, int ny) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    portX = nx;
    portY = ny;
    TR_DEBUG3 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode:: Move(new_x = %d, new_y = %d), this = %s", 
	       portX, portY, Id());
}

int ev_BaseNode::
GetX () {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode::GetX()=%d, this=%s",
	       portX, Id());
    return portX;
}

int ev_BaseNode::
GetY () {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode::GetY()=%d, this=%s",
	       portY, Id());
    return portY;
}

int ev_BaseNode::
NumOfParents () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG2(FLOW_SUPPORT, BIT_2, FALSE,
	      "EXIT  ev_BaseNode::NumOfParents()=%d, this=%s",
	      ParentNode.GetSize(),Id());
    return ParentNode.GetSize ();
}

boolean ev_BaseNode::
AnyParents () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int i = ParentNode.GetSize () - 1;
    while (i >= 0)
        {
            if (ParentNode[i] != SHMNULL)
                {
                    TR_DEBUG1 (FLOW_SUPPORT, BIT_2, FALSE,
                               "EXIT  ev_BaseNode::AnyParents()=TRUE,this=%s",
                               Id());
                    return TRUE;
                }
            i--;
        }
    TR_DEBUG1 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode::AnyParents()=FALSE,this=%s",
	       Id());
    return FALSE;
}

ev_BaseNode *ev_BaseNode::
GetParent (int exit_port) {
    ev_BaseNode *rval = ParentNode[exit_port].dref ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_2, FALSE,
	       "EXIT  ev_BaseNode::GetParent()=%x, this=%s",
	       rval,Id());
    return rval;
}

int ev_BaseNode::
GetCumulativeTraverseCount (int port_index) {
    tr_assert (port_index >= 0);
    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(port_index);
    int rval = ptr -> GetCumulativeTraverseCount();
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  ev_BaseNode::GetCumulativeTraverseCount()=%d, this=%s",
	      rval, Id());
    return rval;
}

void ev_BaseNode::
ResetCumulativeTraverseCounts() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int num_ports = ExitPorts.LargestDefined() + 1;
    for (int ii = 0; ii < num_ports; ii++)
        {
            if (ExitPorts[ii] != SHMNULL)
                {
                    ev_ExitPort *ptr = ExitPorts[ii].dref();
                    ptr -> SetCumulativeTraverseCount(0);
                }
        }
}

void ev_BaseNode::
ResetCumulativeTimers() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    GetCumulativeTimeExpr() -> Jam( 0.0 );
    CumulativeTimer.set(0.0);
}

void ev_BaseNode::
IncTraverseCount(int exit_port) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(exit_port);
    ptr -> IncTraverseCount();
}

int ev_BaseNode::
GetTraverseCount(int exit_port) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(exit_port);
    int rval = ptr -> GetTraverseCount();
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  ev_BaseNode::GetTraverseCount()=%d, this=%s",
	      rval, Id());
    return rval;
	
}

const eVt_timestamp& ev_BaseNode::
StartTimers(const eVt_timestamp&ts) {
    InternalTimer.synchro_start(ts,0.0);
    CumulativeTimer.synchro_start(ts);
    return ts;
};				/* StartTimers() */

eVt_timestamp ev_BaseNode::
StopTimers(const eVt_timestamp& start_time, int exit_port) {
    eVt_timestamp ts = InternalTimer.synchro_stop(
	CumulativeTimer.synchro_stop( ) );
    InternalTime.Jam( InternalTimer.read() );
    CumulativeTime.Jam( CumulativeTimer.read() );

    eV_timer temp;
    temp.synchro_start(start_time);
    temp.synchro_stop( ts );
    GetIntervalTimeExpr(exit_port) -> Jam( temp.read() );

    return ts;
};				/* StopTimers() */


boolean ev_BaseNode::
PutNode (ev_BaseNode * node, int exit_port) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (exit_port >= 0);
    tr_assert (node != NULL);

    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(exit_port);
    boolean rval = ptr -> PutNode( node );
    node -> Connect (this);

    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  ev_BaseNode::PutNode()=%d, this=%s",
	      rval, Id());
    return rval;
}

ev_BaseNode *ev_BaseNode::
GetChild (int exit_port) {
    tr_assert (exit_port >= 0);
    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(exit_port);
    ev_BaseNode *rval = ptr -> GetNode();

    TR_DEBUG2 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  ev_BaseNode::GetChild()=%x, this=%s",
	       rval,Id());
    return rval;
}

void ev_BaseNode::
CutNode (int port_index) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (port_index >= 0);
    ev_ExitPort *ptr = ExitPorts.ObtainExitPort(port_index);
    ev_BaseNode *pp = ptr -> GetNode();

    if (pp != NULL)
			// disconnect this Node from its Ports[port_index]
    {
	pp -> Disconnect (this);
			// Null it out
	ptr -> ResetNode();
    }
    ExitPorts.Zap(port_index);
//        ExitPorts.Compress(); // blast THIS thru, and fix spr.857 4-29-93, 5-13-93 VG
    TR_DEBUG1 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  ev_BaseNode::CutPort(), this=%s",
	       Id());
}
void ev_BaseNode::
DebugDump (FILE * file, int indent) {

    fprintf (file,
	     "%sBaseNode at (%d, %d): %s\n",
	     Indent(indent), portX, portY,Id());
    fprintf (file,
	     "%sParentFlow = %s\n",
	     Indent(indent+1), ParentFlow -> GetFullName ());
    fprintf (file,
	     "%sIndex = %d\n",
	     Indent(indent+1), Index);
    fprintf (file,
	     "%sTag=%s\n",
	     Indent(indent+1), (char*) Tag);
    fprintf (file,
	     "%sExitedPort=%d\n",
	     Indent(indent+1), ExitedPort);
    fprintf (file,
	     "%sInternalTimer=%s\tInternalTime=%s\n",
	     Indent(indent+1), InternalTimer.text(),InternalTime.Id());
    fprintf (file,
	     "%sCumulativeTimer=%s\tCumulativeTime=%s\n",
	     Indent(indent+1), CumulativeTimer.text(),CumulativeTime.Id());
    fprintf (file,
	     "%sExitPorts (MaxNum=%d, MaxRequested=%d):\n",
	     Indent(indent+1), MaxNumExitPorts, RequestedPorts );
    for (int ii=0; ii<=ExitPorts.LargestDefined(); ii++)
	ExitPorts[ii]->DebugDump(file, indent+2);

    switch ((node_type)Type)
    {
      case NODE_FLOW:
	((FlowNode *) this) -> DebugDump (file, indent + 2);
	break;
      case NODE_NUMBER:
	((PortNumber *) this) -> DebugDump (file, indent + 2);
	break;
      case NODE_BIN:
	((Bin *) this) -> DebugDump (file, indent + 2);
	break;
      case NODE_START:
	((StartNode *) this) -> DebugDump (file, indent + 2);
	break;
      default:
	tr_assert (0);
	break;
    }
    for (ii=0; ii<=ParentNode.LargestDefined(); ii++)
    {
	fprintf (file,
		 "%sParentNode[%d]=%s\n",
		 Indent(indent+2), ii, ParentNode[ii]->Id() );
    }
}

/***************************************************************************
			class SpecPair
 ***************************************************************************/

SpecPairArray::
SpecPairArray(const SpecPairArray &array, Flow *parent)
  : Shmptr1dArray() {
    int size = array.GetSize();
    for (int ii = size-1; ii >= 0; ii--)
    {
	if (array[ii] != SHMNULL)
	    (*this)[ii] = *new SpecPair(*array[ii].dref(),parent);
    }
}

SpecPair::
SpecPair (Flow * flow, BaseSymbol * sheet, const char* catnum)
  : BaseSharedObject(),
    ParentFlow(*flow),
    Sheet (*sheet),
    CatNum(*flow, __FILE__,__LINE__) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    CatNum.FixType(EXPR_INTEGER);
    if (catnum != NULL)
	CatNum.Change(catnum);
    tr_assert (flow != NULL);
    tr_assert (flow -> GetType () == EV_FLOW);
    tr_assert (sheet != NULL);
    tr_assert (sheet -> GetType () == EV_SPEC);
    sheet -> AddAncestor (flow);
    TR_DEBUG1(FLOW_SUPPORT, BIT_4, FALSE,
	      "EXIT  SpecPair::SpecPair(), this = %s",
	      Id());
}

SpecPair::
SpecPair(const SpecPair &pair, Flow *flow)
  : BaseSharedObject(),
    ParentFlow(flow),
    Sheet(pair.Sheet),
    CatNum(pair.CatNum, *flow, __FILE__,__LINE__) {
    Sheet.dref()->AddAncestor(flow);
}

SpecPair::
~SpecPair () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Flow *flow = ParentFlow.dref();
    BaseSymbol *sheet = Sheet.dref ();

    sheet -> DelAncestor (flow);
    TR_DEBUG1(FLOW_SUPPORT, BIT_4, FALSE,
	      "EXIT  SpecPair::~SpecPair(), this=%s",
	      Id());
}


BaseSymbol *SpecPair::
GetSheet () {
    BaseSymbol *rval = Sheet.dref ();

    tr_assert (rval != NULL);
    TR_DEBUG2(FLOW_SUPPORT, BIT_6, FALSE,
	      "EXIT  SpecPair::GetSheet()=%s, this=%s",
	      Id(),Id());
    return rval;
}

int SpecPair::
GetCat () {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_6, FALSE,
	       "ENTER SpecPair::GetCat()=%d, this=%s",
	       CatNum, Id());
    
    return CatNum.GetIntValue();
}

Expr* SpecPair::
GetCatExpr () {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_6, FALSE,
	       "ENTER SpecPair::GetCatExpr()=%s, this=%s",
	       CatNum.Id(), Id());
    
    return &CatNum;
}

/***************************************************************************
			class FlowNode
 ***************************************************************************/

FlowNode::
FlowNode (Flow * f, int index)
  : ev_BaseNode(NODE_FLOW, f, index, INT_MAX),
    Exec (),
    SpecCats (), 
    MajorId(),
    AOAContext(),
    CalTabVal(),
    CalIdExpr(),
    BasePerExpr(),
    visited(0),
    FocusCal(EVM_STANDARD_CAL),
    EntryMcl(EV_MclLStatus::StoredStateOk), 
    ExitMcl(EV_MclLStatus::StoredStateOk),
    TestMethodPtr() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    for (int ii = 0; ii < EVg_MAX_WALK_INDEX; ii++) 
	node_visited[ii] = 0;

    Expr *cal_id_ptr = new Expr((char *)EVO_CalId::GetStandardCalIdStr());
    cal_id_ptr->FixType(EXPR_INTEGER);
    CalIdExpr = *cal_id_ptr;
    Stage2Initialize();
    EV_CURTP->GetPTRanges().AddParent(EntryMcl);
			      /* setup the PinType Constraint info to happen at
			      the beginning of each test */

    const char* tstring = EV_CURTP->GetGeneratorString();
    const tint = EV_CURTP->GetNextMajorIdDefault();
    char temp_buf[128];	      /* size if arbitrary */
    sprintf(temp_buf, tstring, tint);
    MajorId.Change(temp_buf);

    TR_DEBUG1 (FLOW_SUPPORT, BIT_8, FALSE,
	       "EXIT FlowNode::FlowNode(), this=%s",
	       Id());
    
}

FlowNode::
FlowNode(const FlowNode &fnode, Flow *flow)
  : ev_BaseNode(fnode,flow),
    Exec(fnode.Exec),
    SpecCats(fnode.SpecCats,flow),
    MajorId(fnode.MajorId,*flow,__FILE__,__LINE__),
    AOAContext(),
    FocusCal(fnode.FocusCal),
    CalTabVal(), visited(fnode.visited), 
    EntryMcl(EV_MclLStatus::StoredStateOk),
    ExitMcl(EV_MclLStatus::StoredStateOk),
    TestMethodPtr(fnode.TestMethodPtr) {
    if ((Exec != SHMNULL) && (Exec->GetName() == NULL))
    {
	switch (Exec->GetType())
	{
	  case EV_TEST:
	    Exec->DelAncestor(GetFlow());
	    Exec = *new EVO_Test(*(EVO_Test *)Exec.dref());
	    ((EVO_Test *)Exec.dref())->GetEntryMcl().AddParent(EntryMcl);
	    ((EVO_Test *)Exec.dref())->GetExitMcl().AddParent(ExitMcl);
	    break;
	  case EV_FLOW:
	    Exec->DelAncestor(GetFlow());
	    Exec = *new Flow(*(Flow *)Exec.dref());
	    break;
	  default:
	    tr_assert(0);
	}
	Exec->AddAncestor(GetFlow());
    }

    for (int ii = 0; ii < EVg_MAX_WALK_INDEX; ii++) 
	node_visited[ii] = 0;

    char temp_buf[128]; // size if arbitrary
    Expr *cal_id_ptr = new Expr((char *)EVO_CalId::GetStandardCalIdStr());
    cal_id_ptr->FixType(EXPR_INTEGER);
    CalIdExpr = *cal_id_ptr;
    Stage2Initialize();
    EV_CURTP->GetPTRanges().AddParent(EntryMcl);
			// setup the PinType Constraint info to happen at the
			// beginning of each test 

    sprintf(temp_buf,EV_CURTP->GetGeneratorString(),
	    EV_CURTP->GetNextMajorIdDefault());
    MajorId.Change(temp_buf);
    TR_DEBUG1(FLOW_SUPPORT, BIT_8, FALSE,
	      "EXIT FlowNode::FlowNode(const FlowNode &),this=%s",
	      Id());
}

FlowNode::
~FlowNode () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

				  /* delete any backarcs to this FlowNodes ParentFlow */
    BaseSymbol *e = Exec.dref ();
    if (e)
	e -> DelAncestor (GetFlow());
    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *ss = SpecCats[ii].dref ();
	if (ss != NULL)
	{
	    delete ss;
	    SpecCats[ii] = SHMNULL;
	}
    }

			// delete any CalIdExpr
    delete CalIdExpr.dref();
    CalIdExpr = SHMNULL;
    CalTabVal.Dealloc();
    EV_CURTP->GetPTRanges().DelParent(EntryMcl);
			// Remove this flow node from this list of parents that
			// the PinTypeConstraint cell list in the TestProg
			// object deals with  
    TR_DEBUG1 (FLOW_SUPPORT, BIT_8, FALSE,
	       "EXIT FlowNode::~FlowNode(), this=%s",
	       Id());
}

int FlowNode::
ExecSpecs (void) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int count = 0;
    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *pair = SpecCats[ii].dref ();
	if (pair != NULL)
	{
	    EVO_Spec *spec = (EVO_Spec *) pair -> GetSheet ();
	    if (spec != NULL)
	    {
		spec -> SetActiveCat (pair -> GetCat ());
		count++;
	    }
	}
    }
    TR_DEBUG2 (GHOST, BIT_8, FALSE,
	       "EXIT  FlowNode::ExecSpecs()=%d, this=%s",
	       count, Id());
    return count;
}


EVM_FocusCal FlowNode::
GetFocusCal() {
    EVM_FocusCal rval = FocusCal;
//    if (FocusCal == EVM_DEFAULT_CAL) {
//	BaseSymbol *test = Exec.dref();
//	if ((test != NULL) && (test->GetType() == EV_TEST))
//	    rval = ((EVO_Test *)test)->GetFocusCal();
//    }
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  FlowNode::GetFocusCal()=%d,this=%s",
	      rval,Id());
    return rval;
}

boolean FlowNode::
SetCalTId(const EVO_CalId &id) {
    TR_DEBUG0 ( TIMING_CAL, BIT_22, 0,
		"Enter FlowNode::SetCalTId()");
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    EV_ASSERT1(((EVO_CalId&)id).Valid(), "Invalid CalTable: %d",id);

    char id_buf[128];	// 128 is arbitrary. This is a buffer where an
			// expression is generated. This expression is used to
			// decide whether to use the CalTable of this FlowNode,
			// or zero (which means Standard Calibration). If the
			// Parent Flow's StandardCal only expression is set,
			// Standard Cal is used, otherwise, the local CalTable
			// is used. 

    Expr *cal_id_ptr = CalIdExpr.dref();
    tr_assert(cal_id_ptr != NULL);

    CalTabVal.Dealloc();
    CalTabVal = id;

    sprintf(id_buf,"if(%s.%s,%s,%d)",
	    GetFlow()->GetFullName(),
	    STANDARD_CALIBRATION_EXPR,
	    EVO_CalId::GetStandardCalIdStr(),
	    (int)CalTabVal);
    
    TR_DEBUG1(FLOW_SUPPORT, BIT_11, FALSE,
	      "\tSetting CalIdExpr = \"%s\"",
	      id_buf);
    
    boolean rval = cal_id_ptr->Change(id_buf);
    
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  FlowNode::SetCalTId()=%s, this=%s",
	      BooleanName(rval),Id());
    TR_DEBUG1 ( TIMING_CAL, BIT_22, 0,
		"Exit FlowNode::SetCalTId(%3d)",
		rval);
    return rval;
}

boolean FlowNode::
AllocCalTId() {
    TR_DEBUG0 ( TIMING_CAL, BIT_22, 0,
		"Enter FlowNode::AllocCalTId()");
    EVO_CalId id;
    id.Alloc();
    TR_DEBUG0 ( TIMING_CAL, BIT_22, 0,
		"Exit  FlowNode::AllocCalTId()");
    return SetCalTId(id);
}

boolean FlowNode::
DeallocCalTId() {
    TR_DEBUG0 ( TIMING_CAL, BIT_22, 0, "Enter FlowNode::DeallocCalTId()");
    if (CalTabVal.Valid())
	CalTabVal.Dealloc();
    char id_buf[128];
    TR_DEBUG0 ( TIMING_CAL, BIT_22, 0, "Exit  FlowNode::DeallocCalTId()");
    return CalIdExpr->Change(EVO_CalId::GetStandardCalIdStr());
}
                                               
void FlowNode::
PutExec (BaseSymbol * ee) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    EV_ASSERT0(ee != NULL, "Can't put a NULL exec object");

    ev_type type = ee->GetType();
    EV_ASSERT1((type == EV_TEST) ||
	       (type == EV_FLOW) ||
	       (type == EV_PLOT) ||
	       (type == EV_NULL_SYMBOL),
	       "Illegal type %s for FlowNode Execute object",
	       ev_GetTypeStr(ee->GetType()));

    BaseSymbol *exec = Exec.dref ();

    if (exec != NULL)
    {
	exec -> DelAncestor (GetFlow());
	if (exec->GetType() == EV_TEST)
	{
	    ((EVO_Test *)exec)->GetEntryMcl().DelParent(EntryMcl);
	    ((EVO_Test *)exec)->GetExitMcl().DelParent(ExitMcl);
	}
    }

    Exec = *ee;
    ee->AddAncestor(GetFlow());
    if (ee->GetType() == EV_TEST)
    {
	((EVO_Test *)ee)->GetEntryMcl().AddParent(EntryMcl);
	((EVO_Test *)ee)->GetExitMcl().AddParent(ExitMcl);
    }
    TR_DEBUG1 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  FlowNode::PutExec(), this = %s",
	       Id());
}

const char *FlowNode::
GetExec () {
    BaseSymbol *e = Exec.dref ();
    const char *rval;
    if (e)
	rval = e -> GetName ();
    else
	rval = NULL;

    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  FlowNode::GetExec()=%s, this=%s",
	      rval, Id());
    return rval;
}

BaseSymbol *FlowNode::
GetExecPtr () {
    BaseSymbol *rval = Exec.dref ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  FlowNode::GetExecPtr()=%x, this=%s",
	       rval, Id());
    return rval;
}


boolean FlowNode::
AddSpecPair (BaseSymbol * sheet, const char *cat) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (sheet != NULL);
    tr_assert (sheet -> GetType () == EV_SPEC);

    boolean rval = FALSE;
    int size = SpecCats.GetSize ();
    boolean done = FALSE;

    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *ss = SpecCats[ii].dref ();
	if ((ss != NULL) && (sheet == ss->GetSheet()))
	{
	    delete ss;
	    SpecCats[ii] = SHMNULL;
	    break;
	}
    }
    SpecPair *ss = new SpecPair (GetFlow(), sheet, cat);
    SpecCats[ii] = *ss;
    rval = TRUE;

    TR_DEBUG2 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  FlowNode::AddSpecPair()=%d,this=%s",
	       rval, Id());
    return rval;
}

void FlowNode::
DelSpecPair (BaseSymbol * sheet) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (sheet != NULL);
    tr_assert (sheet -> GetType () == EV_SPEC);

    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *ss = SpecCats[ii].dref ();
	if (ss != NULL)
	{
	    if (ss -> GetSheet () == sheet)
	    {
		delete ss;
		SpecCats[ii] = SHMNULL;
	    }
	}
    }
    TR_DEBUG1 (FLOW_SUPPORT, BIT_10, FALSE,
	       "EXIT  FlowNode::DelSpecPair(), this=%s",
	       Id());
}

int FlowNode::
GetCatNum (BaseSymbol * sheet) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (sheet != NULL);
    tr_assert (sheet -> GetType () == EV_SPEC);

    int rval = -1;
    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *ss = SpecCats[ii].dref ();
	if (ss != NULL)
	{
	    EVO_Spec *spec = (EVO_Spec *) ss -> GetSheet ();

	    if (spec != NULL)
	    {
		EV_ASSERT8 (spec -> GetType () == EV_SPEC, "FlowNode::GetCatNum(%s) [%s, MajorId=%s]:\n"
				"SpecCats[%d]->GetSheet()=%08x, %s, GetType=%d",
				 sheet->Id(), Id(), GetMajorIdString(), ii, ss, spec, spec->Id(), spec->GetType() );
		if (spec == sheet)
		{
		    rval = ss -> GetCat ();
		    break;
		}
	    }
	}
    }
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  FlowNode::GetCatNum()=%d,this=%s",
	      rval, Id());
    return rval;
}

Expr* FlowNode::
GetCatExpr(BaseSymbol *sheet) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (sheet != NULL);
    tr_assert (sheet -> GetType () == EV_SPEC);

    Expr* rval = NULL;
    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *ss = SpecCats[ii].dref ();
	if (ss != NULL)
	{
	    EVO_Spec *spec = (EVO_Spec *) ss -> GetSheet ();
	    if (spec != NULL)
	    {
		tr_assert (spec -> GetType () == EV_SPEC);
		if (spec == sheet)
		{
		    rval = ss -> GetCatExpr ();
		    break;
		}
	    }
	}
    }
    TR_DEBUG2(FLOW_SUPPORT, BIT_10, FALSE,
	      "EXIT  FLowNode::GetCatExpr()=%08x,this=%s",
	      rval, Id());
    
    return rval;
}
    
evm_MethodPtrRec& FlowNode::
GetTestMethodPtr() {
    return TestMethodPtr;
}

void FlowNode::
DebugDump (FILE * file, int indent) {
    BaseSymbol *symbol = Exec.dref ();

    if (symbol != NULL)
    {
	fprintf (file,
		 "%sExecuteObject = %s\n",
		 Indent(indent), symbol->Id ());
    }
    else
    {
	fprintf (file, "%sExecuteObject = (none)\n", Indent(indent));
    }
    fprintf(file,"%svisited=%s\n",Indent(indent), visited);
    fprintf(file,"%sMajorId=%s\n",Indent(indent), MajorId.Id());
    fprintf(file,"%sFocusCal=%d\n",Indent(indent), FocusCal );
    fprintf(file,"%sCalTable=%d\n",Indent(indent), (int)CalTabVal );
    fprintf(file,"%sCalIdExpr=%s\n",Indent(indent), CalIdExpr->Id() );
    fprintf(file,"%sBasePerExpr=%s\n",Indent(indent), BasePerExpr->Id() );
    fprintf(file,"HEY!! - should dump out AOAContext!!\n", Indent(indent) );

    fprintf (file, "%sSpecPairs:\n", Indent(indent));
    int size = SpecCats.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	SpecPair *pair = SpecCats[ii].dref ();
	if (pair != NULL)
	{
	    BaseSymbol *bb = pair -> GetSheet ();
	    fprintf (file,
		     "%s[%d]: (%s, %s)\n",
		     Indent(indent+1),
		     ii,
		     bb -> GetName (),
		     pair->GetCatExpr()->Id());
	}
    }
    fprintf(file,"%sEntryMcl:\n",Indent(indent));
    EntryMcl.DebugDump(file,indent+1);
    fprintf(file,"%sExitMcl:\n",Indent(indent));
    ExitMcl.DebugDump(file,indent+1);

}


/***************************************************************************
			class PortNumber
 ***************************************************************************/

PortNumber::
PortNumber (Flow * f, int index)
  : ev_BaseNode(NODE_NUMBER, f, index, 1),
    Number(-1) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    TR_DEBUG1(FLOW_SUPPORT, BIT_12, FALSE,
	      "EXIT  PortNumber::PortNumber(), this=%s",
	      Id());
}

PortNumber::
PortNumber(const PortNumber &number, Flow *flow)
  : ev_BaseNode(number,flow),
    Number(number.Number) {
}

PortNumber::
~PortNumber () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG1 (FLOW_SUPPORT, BIT_12, FALSE,
	       "EXIT  PortNumber::~PortNumber(), this=%s",
	       Id());
}

void PortNumber::
SetPortNumber (int port_number) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Number = port_number;
    TR_DEBUG1 (FLOW_SUPPORT, BIT_14, FALSE,
	       "EXIT  PortNumber::SetPortNumber(), this=%s",
	       Id());
}

int PortNumber::
GetPortNumber () {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_14, FALSE,
	       "EXIT  PortNumber::GetPortNumber()=%d, this=%s",
	       Number,Id());
    return Number;
}

void PortNumber::
DebugDump (FILE * file, int indent) {
    fprintf (file,
	     "%sPortNumber = %d\n",
	     Indent(indent), Number);
}

/***************************************************************************
			class Bin
 ***************************************************************************/

Bin::
Bin (Flow * f, int index)
  : ev_BaseNode(NODE_BIN, f, index, 1),
    Number(-1) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    TR_DEBUG1 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::Bin(), this=%s",
	       Id());
}

Bin::
Bin(const Bin &bin, Flow *flow)
  : ev_BaseNode(bin,flow),
    Number(bin.Number) {
}

Bin::
~Bin () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG1 (FLOW_SUPPORT, BIT_16, FALSE, "EXIT  Bin::~Bin()", Id());
}

void Bin::
SetBinNumber (int bin_number) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Number = bin_number;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();

	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();

	if (bin == NULL)
	{
	    bin = new EV_SoftBin (Number);
	    (*array)[Number] = *bin;
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::SetBinNumber(), this=%s",
	      Id());
}

int Bin::
GetBinNumber () {
    TR_DEBUG2(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::GetBinNumber()=%d, this=%s",
	      Number,Id());
    return Number;
}


void Bin::
IncCount () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG1 (FLOW_SUPPORT, BIT_17, FALSE,
	       "\tSoftBinNumber = %d",
	       Number);
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();

	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();

	if ( bin != NULL )  // added 12-1-93 VG
	{
	    TR_DEBUG1 (FLOW_SUPPORT, BIT_17, FALSE,
		       "\tIncrementing count on soft bin %d",
		       Number);
	    bin -> IncCount ();
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_16, FALSE, "EXIT  Bin::IncCount(),this=%s", Id());
}


const char *Bin::
GetBinName () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    const char *rval = NULL;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();

	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();

	if ( bin != NULL )  // added 12-1-93 VG
	{
	    TR_DEBUG1 (FLOW_SUPPORT, BIT_17, FALSE,
		       "\tGetting name for soft bin %d",
		       Number);
	    rval=bin->GetName ();
	}
    }
    TR_DEBUG2(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::GetBinName()=%s, this=%s",
	      rval, Id());
    return rval;
}

const char *Bin::
GetBinDescript () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    const char *rval = NULL;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();

	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();

	if ( bin != NULL )  // added 12-1-93 VG
	{
	    rval = bin -> GetDescript ();
	}
    }
    TR_DEBUG2(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::GetBinDescript()=%s, this=%s",
	      rval,Id());
    return rval;
}

const char *Bin::
GetXColor () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    const char *rval;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();

	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();

	if (bin != NULL)  // added 12-1-93 VG
	{
	    TR_DEBUG1 (FLOW_SUPPORT, BIT_17, FALSE,
		       "\tGetting xcolor for soft bin %d",
		       Number);
	    rval = bin -> GetXColor ();
	}
    }
    TR_DEBUG2 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::GetXColor()=%s, this=%s",
	       rval, Id());
    return rval;
}

int Bin::
GetXColorIndex () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval = -1;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    rval = bin -> GetXColorMapIndex ();
	}
    }

    TR_DEBUG2 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::GetXColorIndex()=%d, this=%s",
	       rval, Id());
    return rval;
}

int Bin::
GetBinCount () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval=-1;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    rval = bin -> GetCount ();
	}
    }
    TR_DEBUG2 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::GetBinCount()=%d, this=%s",
	       rval,Id());
    return rval;
}

int Bin::
GetMaxCount () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval = -1;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    rval = bin -> GetMaxCount ();
	}
    }
    TR_DEBUG2 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::GetMaxCount()=%d, this=%s",
	       rval, Id());
    return rval;
}

int Bin::
GetHWBinNumber () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval=-1;
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    rval = bin -> GetHWBin ();
	}
    }

    TR_DEBUG2 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::GetHWBinNumber()=%d, this=%s",
	       rval, Id());
    return rval;
}

void Bin::
ChangeBinName (const char *name) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    bin -> PutName ((char*)name);
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::ChangeBinName(), this=%s",
	      Id());
}

void Bin::
ChangeBinDescript (const char *descript) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    bin -> PutDescript ((char*)descript);
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::ChangeBinDescript(), this=%s",
	      Id());
}

void Bin::
ChangeXColor (const char *color) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    bin -> PutXColor ((char*)color);
	}
    }
    TR_DEBUG1 (FLOW_SUPPORT, BIT_16, FALSE,
	       "EXIT  Bin::ChangeXColor(), this=%s",
	       Id());
}

void Bin::
ChangeXColorIndex (int index) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP -> GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    bin -> PutXColorMapIndex (index);
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_16, FALSE,
	      "EXIT  Bin::ChangeXColorIndex(), this=%s",
	      Id());
}

void Bin::
DebugDump (FILE * file, int indent) {
    fprintf (file, "%sBinNumber=%d\n", Indent(indent), Number);
    if (Number >= 0)
    {
	EV_SoftBinArray *array = EV_CURTP->GetSoftBinArray ();
	tr_assert (array != NULL);
	EV_SoftBin *bin = (*array)[Number].dref ();
//	tr_assert (bin != NULL);
	if ( bin != NULL )  // added 12-1-93 VG
	{
	    bin->DebugDump(file, indent+1);
	}
    }
}

/***************************************************************************
			class StartNode
 ***************************************************************************/

StartNode::
StartNode (Flow * flow, int index)
  : ev_BaseNode(NODE_START, flow, index, 1),
    EntryPoint(EVF_NOT_ENTRY_POINT) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG1(FLOW_SUPPORT, BIT_20, FALSE,
	      "EXIT  StartNode::StartNode(), this=%s",
	      Id());
}

StartNode::
StartNode(const StartNode &node, Flow *flow)
  : ev_BaseNode(node,flow),
    EntryPoint(node.EntryPoint) {
}

StartNode::
~StartNode() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    if ((EntryPoint >= EVF_ON_START) &&
	(EntryPoint <= EVF_USR_DEF_9))
    {
	Flow *flow = GetFlow();   // this removes this StartNode from the
	if ( flow != NULL )       // Flow EntryPoint array for SPR.541
	    flow->ChangeStart( EntryPoint );  // 9-29-92 VG
    }

    TR_DEBUG1 (FLOW_SUPPORT, BIT_20, FALSE,
	       "EXIT  StartNode::~StartNode(), this=%s",
	       Id());
}


void StartNode::
SetAsEntryPoint (evf_flow_entry_point entry) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Flow *flow = GetFlow();
    tr_assert (flow != NULL);

    if ((EntryPoint >= EVF_ON_START) &&
	(EntryPoint <= EVF_USR_DEF_9))
    {
	flow->ChangeStart(EntryPoint);
			// This clears its former position
			// in the Flow EntryPoint array, and fixes SPR.541
			// 9-29-92 VG
    }

    evf_flow_entry_point tentry = entry;
    if (tentry != EVF_NOT_ENTRY_POINT)
	flow -> ChangeStart (entry, this);
    else
	flow -> ChangeStart (entry);
    EntryPoint = tentry;
    TR_DEBUG1 (FLOW_SUPPORT, BIT_22, FALSE,
	       "EXIT  StartNode::SetEntryPoint(1v), this=%s",
	       Id());
}


void StartNode::
UnSetAsEntryPoint () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    EntryPoint = EVF_NOT_ENTRY_POINT;
    TR_DEBUG1(FLOW_SUPPORT, BIT_22, FALSE,
	      "EXIT  StartNode::UnSetAsEntryPoint(), this=%s",
	      Id());
}

evf_flow_entry_point StartNode::
GetEntryPoint () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    return EntryPoint;
}


void StartNode::
DebugDump (FILE * file, int indent) {
    fprintf (file,
	     "%sEntryPoint = %d\n",
	     Indent(indent), EntryPoint);
}

/***************************************************************************
			class Flow
 ***************************************************************************/

inline const SharedString*
GetSharedString(const char*string) {
    return EV_CURTP -> GetSharedStringTable ().Submit (EV_TEST, string);
};

boolean Flow::
IsFlowToolUp() {
    if ( FlowToolUp[0] == 0 )
        return FALSE;
    return TRUE; // True if flowtool is up.
}

void Flow::
SetFlowTooUp( boolean ToolUp  ) {
    if ( FlowToolUp.GetSize() == 0 )
        FlowToolUp.PreSize( 1 );

    if ( ToolUp )
        FlowToolUp[0] = 1;
    else
        FlowToolUp[0] = 0;
}

Flow::
Flow (const char *name)
  : BaseSymbol((char*)name,EV_FLOW),
    EntryPoint (),
    EntryPointStat (), 
    LoopExpr (),
    DoCalWhen(),
    StandardCalOnly(),
    NumberOfNodes(0),
    NumberOfBacks(0),
    UserDefaultCalId(-1),
    NotifyOnTest(FALSE),
    WhichEntry(EVF_ON_START),
    NumOfExecs(0),
    RunNumber(0),
    EnableTimers(*this,__FILE__,__LINE__),
    CumulativeTimer(),
    CumulativeTime(*this,__FILE__,__LINE__),
    LastExecTimer(),
    LastExecTime(*this,__FILE__,__LINE__),
    NodeTable(32,32),
    FreeNodes(),
    FlowBack(),
    FreeBacks() {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Expr *ee = new Expr ();
    ee -> FixType (EXPR_BOOLEAN);
    LoopExpr = ee -> GetShmAddr ();

    SharedString StdCalName = *GetSharedString(STANDARD_CALIBRATION_EXPR);
    Expr *std_cal = new Expr(*this,&StdCalName);
    std_cal->FixType(EXPR_BOOLEAN);
    StandardCalOnly = *std_cal;
    SetStandardCalOnly(FALSE);

    EnableTimers.FixType(EXPR_BOOLEAN);
    EnableTimers.Change("TRUE");

    CumulativeTime.JamType("S");
    CumulativeTime.SetOutputExpr();
    CumulativeTime.Change("#");
    CumulativeTime.Jam(0.0);

    LastExecTime.JamType("S");
    LastExecTime.SetOutputExpr();
    LastExecTime.Change("#");
    LastExecTime.Jam(0.0);
    LastExecTime.SetPersistance(PERSIST_DEVICE);

    for (int ii = 0; ii < EVg_MAX_WALK_INDEX; ii++) 
	walk_counters[ii] = 0;
    TR_DEBUG1(FLOW_SUPPORT, BIT_28, FALSE, "EXIT  Flow::Flow(), this=%s",Id());
}

Flow::
Flow ()
  : BaseSymbol(EV_FLOW),
    EntryPoint (),
    EntryPointStat (),
    LoopExpr (), 
    NodeTable (32, 32),
    FreeNodes (),
    FlowBack (),
    FreeBacks (),
    DoCalWhen(), 
    StandardCalOnly(),
    NumberOfNodes(0),
    NumberOfBacks(0),
    NotifyOnTest(FALSE),
    UserDefaultCalId(-1),
    WhichEntry(EVF_ON_START),
    NumOfExecs(0),
    EnableTimers(*this,__FILE__,__LINE__),
    CumulativeTimer(),
    CumulativeTime(*this,__FILE__,__LINE__),
    LastExecTimer(),
    LastExecTime(*this,__FILE__,__LINE__) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Expr *ee = new Expr ();
    ee -> FixType (EXPR_BOOLEAN);
    LoopExpr = ee -> GetShmAddr ();

    SharedString StdCalName = *GetSharedString(STANDARD_CALIBRATION_EXPR);
    Expr *std_cal = new Expr(*this,&StdCalName);
    std_cal->FixType(EXPR_BOOLEAN);
    StandardCalOnly = *std_cal;
    SetStandardCalOnly(FALSE);

    EnableTimers.FixType(EXPR_BOOLEAN);
    EnableTimers.Change("TRUE");

    CumulativeTime.JamType("S");
    CumulativeTime.SetOutputExpr();
    CumulativeTime.Change("#");
    CumulativeTime.Jam(0.0);

    LastExecTime.JamType("S");
    LastExecTime.SetOutputExpr();
    LastExecTime.Change("#");
    LastExecTime.Jam(0.0);
    LastExecTime.SetPersistance(PERSIST_DEVICE);
    for (int ii = 0; ii < EVg_MAX_WALK_INDEX; ii++) 
	walk_counters[ii] = 0;
    TR_DEBUG1 (FLOW_SUPPORT, BIT_0, FALSE, "EXIT  Flow::Flow(),this=%s", Id());
}

Flow::
Flow(const Flow &flow)
  : BaseSymbol(flow),
    EntryPoint(),
    WhichEntry(flow.WhichEntry),
    EntryPointStat(flow.EntryPointStat),
    NumOfExecs(flow.NumOfExecs),
    NodeTable(), FreeNodes(flow.FreeNodes),
    NumberOfNodes(flow.NumberOfNodes),
    FlowBack(flow.FlowBack),
    FreeBacks(flow.FreeBacks),
    NumberOfBacks(flow.NumberOfBacks),
    UserDefaultCalId(-1),
    LoopExpr(),
    NotifyOnTest(flow.NotifyOnTest),
    RunNumber(0),
    StandardCalOnly(),
    DoCalWhen(flow.DoCalWhen),
    CalFile(),
    EnableTimers(flow.EnableTimers,*this,__FILE__,__LINE__),
    CumulativeTimer(),
    CumulativeTime(flow.CumulativeTime,*this,__FILE__,__LINE__),
    LastExecTimer(),
    LastExecTime(flow.LastExecTime,*this,__FILE__,__LINE__) {
    TR_DEBUG2 (FLOW_SUPPORT, BIT_28, FALSE,
	       "ENTER Flow::Flow(%s),this=%s",
	       flow.Id(),Id());

    StandardCalOnly = new Expr(*flow.StandardCalOnly.dref(),
			       *this,
			       GetSharedString(STANDARD_CALIBRATION_EXPR));

    TR_DEBUG0 (FLOW_SUPPORT, BIT_29, FALSE,
	       "\tDealing with entrypoints");
    int size = flow.EntryPoint.GetSize();
    for (int ii = 0; ii < size; ii++)
			// deal with the entrypoints
    {
	if (flow.EntryPoint[ii] != SHMNULL)
	{
	    ev_BaseNode *pp = flow.EntryPoint[ii].dref();
	    EV_ASSERT0(pp->GetType() == NODE_START,
		       "Illegal start node type");
	    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
		       "\t\tEntryPoint[%d] = %s",ii,pp->Id());
	    EntryPoint[ii] = *new StartNode(*(StartNode *)pp,this);
	}
    }

    TR_DEBUG0 (FLOW_SUPPORT, BIT_29, FALSE, "\tDealing with Ports");
    size = flow.NodeTable.GetSize();
    for (ii = 0; ii < size; ii++)
			// deal with the rest of the flow nodes
    {
	if (flow.NodeTable[ii] != SHMNULL)
	{
	    ev_BaseNode *pp = flow.NodeTable[ii].dref();
	    switch(pp->GetType())
	    {
	      case NODE_FLOW:
		TR_DEBUG0 (FLOW_SUPPORT, BIT_30, FALSE,
			   "\t\tMaking new FlowNode");
		NodeTable[ii] = *new FlowNode(*(FlowNode *)pp,this);
		break;
	      case NODE_NUMBER:
		TR_DEBUG0 (FLOW_SUPPORT, BIT_30, FALSE,
			   "\t\tMaking new PortNumber");
		NodeTable[ii] = *new PortNumber(*(PortNumber *)pp,this);
		break;
	      case NODE_BIN:
		TR_DEBUG0 (FLOW_SUPPORT, BIT_30, FALSE,
			   "\t\tMaking new Bin");
		NodeTable[ii] = *new Bin(*(Bin *)pp,this);
		break;
	      case NODE_START:
		TR_DEBUG0 (FLOW_SUPPORT, BIT_30, FALSE,
			   "\t\tMaking new StartNode");
		NodeTable[ii] = *new StartNode(*(StartNode *)pp,this);
		break;
	      default:
		tr_assert(0);
	    }
	}
    }

    TR_DEBUG0 (FLOW_SUPPORT, BIT_29, FALSE,
	       "\tDealing with EntryPoint Connections");
    size = flow.EntryPoint.GetSize();
    for (ii = 0; ii < size; ii++)
    {
	if (flow.EntryPoint[ii] != SHMNULL)
	{
	    StartNode *p1 = (StartNode*)flow.EntryPoint[ii].dref();
	    StartNode *p2 = (StartNode*)EntryPoint[ii].dref();
	    EV_ASSERT0(p1->GetType() == NODE_START,
		       "Illegal start node type");
	    ev_BaseNode *tmp = p1->GetNode();
			// Next Node in the old flow
	    if (tmp != NULL)
	    {
		int which_connect = tmp->GetIndex();
			// What is its index?
		TR_DEBUG3 (FLOW_SUPPORT, BIT_31, FALSE,
			   "\t\t\tStartNode[%d][0]->Node[%d]=%s",
			   ii,which_connect,
			   EntryPoint[which_connect].dref()->Id());
		p2->PutNode(NodeTable[which_connect].dref());
	    }
	}
    }

    TR_DEBUG0 (FLOW_SUPPORT, BIT_29, FALSE,
	       "\tDealing with Port Connections");
    size = flow.NodeTable.GetSize();
    for (ii = 0; ii < size; ii++)
			// deal with the connections
    {
	if (flow.NodeTable[ii] != SHMNULL)
	{
	    ev_BaseNode *p1 = flow.NodeTable[ii].dref();
	    ev_BaseNode *p2 = NodeTable[ii].dref();
	    switch(p1->GetType())
	    {
	      case NODE_FLOW:
		{
		    FlowNode *f1 = (FlowNode *)p1;
		    FlowNode *f2 = (FlowNode *)p2;
		    int num_children = f1->NumOfChildren();
		    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
			       "\t\tNode[%d] is FlowNode, doing Connections:",ii);
		    for (int jj = 0; jj < num_children; jj++)
		    {
			ev_BaseNode *tmp = f1->GetChild(jj);
			if (tmp != NULL)
			{
			    int which_connect = tmp->GetIndex();
			    TR_DEBUG4 (FLOW_SUPPORT, BIT_31, FALSE,
				       "\t\t\tFlowNode[%d][%d]->Node[%d]=%s",
				       ii,jj,
				       which_connect,
				       NodeTable[which_connect].dref()->Id());
			    f2->PutNode(NodeTable[which_connect].dref(),jj);
			}
		    }
		}
		break;
	      case NODE_BIN:
		{
		    Bin *b1 = (Bin *)p1;
		    Bin *b2 = (Bin *)p2;
		    ev_BaseNode *tmp = b1->GetNode();
		    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
			       "\t\tNode[%d] is Bin, doing Connections:",ii);
		    if (tmp != NULL)
		    {
			int which_connect = tmp->GetIndex();
			TR_DEBUG3 (FLOW_SUPPORT, BIT_31, FALSE,
				   "\t\t\tBin[%d][0]->Node[%d]=%s",
				   ii,
				   which_connect,
				   NodeTable[which_connect].dref()->Id());
			b2->PutNode(NodeTable[which_connect].dref());
		    }
		}
		break;
	      case NODE_START:
		{
		    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
			       "\t\tNode[%d] is StartNode, doing Connections:",ii);
		    StartNode *s1 = (StartNode *)p1;
		    StartNode *s2 = (StartNode *)p2;
		    ev_BaseNode *tmp = s1->GetNode();
		    if (tmp != NULL)
		    {
			int which_connect = tmp->GetIndex();
			TR_DEBUG3 (FLOW_SUPPORT, BIT_31, FALSE,
				   "\t\t\tStartNode[%d][0]->Node[%d]=%s",
				   ii,
				   which_connect,
				   NodeTable[which_connect].dref()->Id());
			s2->PutNode(NodeTable[which_connect].dref());
		    }
		}
		break;
	      case NODE_NUMBER:
		TR_DEBUG0 (FLOW_SUPPORT, BIT_30, FALSE,
			   "\t\tNode[%d] is PortNumber, doing nothing");
		break;
	      default:
		tr_assert(0);
	    }
	}
    }
    TR_DEBUG0 (FLOW_SUPPORT, BIT_29, FALSE,
	       "\tDealing with LoopExpr");
    Expr *ee = flow.LoopExpr.dref();
    if (ee != NULL)
	LoopExpr = *new Expr(*ee,*this);
    TR_DEBUG1 (FLOW_SUPPORT, BIT_28, FALSE,
	       "EXIT  Flow::Flow(),this=%s", Id());
}

Flow::
~Flow () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int size = NodeTable.GetSize ();
    for (int ii = 0; ii < size; ii++)
	DelNode (ii);
    NodeTable.Compress ();

    delete LoopExpr.dref();
    LoopExpr = SHMNULL;

    size = EntryPoint.GetSize ();
    for (ii = 0; ii < size; ii++)
			// all of these ports should already be deleted from the
			// above loop.
    {
	EntryPoint[ii] = SHMNULL;
    }
    EntryPoint.Compress ();

    delete StandardCalOnly.dref();
    StandardCalOnly = SHMNULL;

    current_test_program -> UnSetActiveFlow (this);
    TR_DEBUG1 (FLOW_SUPPORT, BIT_28, FALSE, "EXIT  Flow::~Flow(), this=%s", Id());
}

int Flow::
GetExitedPort () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG2(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::GetExitedPort()=%d, this=%s",
	      ExitedPort,Id());
    return ExitedPort;
}

int Flow::
GetNumOfExecs () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::NumOfExecs()=%d, this=%s",
	       NumOfExecs,Id());
    return NumOfExecs;
}

int Flow::
GetEntryPointStat (evf_flow_entry_point which) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval=-1;
    if ((which >= EVF_ON_START) && (which <= EVF_USR_DEF_9))
	rval = EntryPointStat[which];
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetEntryPointStat()=%d, this=%s",
	       rval,Id());
    return rval;
}

ev_BaseNode *Flow::
AllocNode (node_type type) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int size = NodeTable.GetSize ();
    int ii = 0;
    boolean found = FALSE;

    int where = FreeNodes.Pop ();
    if (where == INT_MIN)
	where = NumberOfNodes++;
    switch (type)
    {
      case NODE_FLOW:
	{
	    FlowNode * fnode = new FlowNode (this, where);
	    NodeTable[where] = *fnode;
	    return fnode;
	}
      case NODE_NUMBER:
	{
	    PortNumber * number = new PortNumber (this, where);
	    NodeTable[where] = *number;
	    return number;
	}
      case NODE_BIN:
	{
	    Bin * bin = new Bin (this, where);
	    NodeTable[where] = *bin;
	    return bin;
	}
      case NODE_START:
	{
	    StartNode * snode = new StartNode (this, where);
	    NodeTable[where] = *snode;
	    return snode;
	}
      default:
	tr_assert (0);
	break;
    }
}

void Flow::
DelNode (int which) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (which >= 0);
    ev_BaseNode *node = NodeTable[which].dref ();

    if (node != NULL)
    {
	node_type type = node -> GetType ();

	switch (type)
	{
	  case NODE_FLOW:
	    delete (FlowNode *) node;
	    break;
	  case NODE_NUMBER:
	    delete (PortNumber *) node;
	    break;
	  case NODE_BIN:
	    delete (Bin *) node;
	    break;
	  case NODE_START:
	    delete (StartNode *) node;
	    break;
	  default:
	    tr_assert (0);
	}
	NodeTable[which] = SHMNULL;
	FreeNodes.Push (which);
    }
}

void Flow::
ChangeStart (evf_flow_entry_point entry, ev_BaseNode * p) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    ev_BaseNode *tp = p;
    if (tp != NULL)
    {
	if ((entry >= EVF_ON_START) && (entry <= EVF_USR_DEF_9))
	{
	    StartNode *node = (StartNode *) EntryPoint[entry].dref ();

	    if (node != NULL)
		node -> UnSetAsEntryPoint ();
	    if (tp -> GetType () == NODE_START)
		EntryPoint[entry] = *tp;
	    else
		EntryPoint[entry] = SHMNULL;
	}
    }
    else
    {
	if ((entry >= EVF_ON_START) && (entry <= EVF_USR_DEF_9))
	    EntryPoint[entry] = SHMNULL;
    }
    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::ChangeStart(), this=%s",
	       Id());
}

ev_BaseNode *Flow::
GetStart (evf_flow_entry_point entry) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert ((entry >= EVF_ON_START) && (entry <= EVF_USR_DEF_9));
    ev_BaseNode *rval = EntryPoint[entry].dref ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetStart()=%d, this=%s",
	       rval, Id());
    return rval;
}

void Flow::
SetEntryPoint (evf_flow_entry_point entry) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert ((entry >= EVF_ON_START) && (entry <= EVF_USR_DEF_9));
    WhichEntry = entry;
    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::SetEntryPoint(), this=%s",
	       Id());
}

evf_flow_entry_point Flow::
GetCurrentEntryPoint () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetCurrentEntryPoint()=%d, this=%s",
	       WhichEntry,Id());
    return WhichEntry;
}

ev_BaseNode *Flow::
GetNode (int i) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (i >= 0);
    ev_BaseNode *rval = NodeTable[i].dref ();
    TR_DEBUG2(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::GetNode()=%x, this=%x",
	      rval, Id());
    return rval;
}

int Flow::
GetNodeTableSize () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    int rval = NodeTable.GetSize ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetNodeTableSize()=%d, this=%s",
	       rval, Id());
    return rval;
}

void Flow::
PutBackInt (int index, int which, int data) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (index >= 0);
    tr_assert (which >= 0);
    Background *b = FlowBack[index].dref ();
    if (!b)
    {
	b = new Background ();
	FlowBack[index] = *b;
    }
    b -> PutInt (which, data);
    TR_DEBUG1(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::PutBackInt(), this=%s",
	      Id());
}

int Flow::
GetBackInt (int index, int which) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (index >= 0);
    tr_assert (which >= 0);
    int rval = TR_NO_INT;
    Background *b = FlowBack[index].dref ();
    if (b)
	rval = b -> GetInt (which);
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetBackInt()=%d, this=%s",
	       rval,Id());
    return rval;
}

void Flow::
PutBackStr (int index, const char *str) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (index >= 0);
    tr_assert (str);
    Background *b = FlowBack[index].dref ();
    if (!b)
    {
	b = new Background;
	FlowBack[index] = *b;
    }
    b -> PutString ((char*)str);
    TR_DEBUG1(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::PutBackStr(), this=%s",
	      Id());
}

const char *Flow::
GetBackStr (int index) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (index >= 0);
    const char *rval = NULL;
    Background *b = FlowBack[index].dref ();
    if (b)
	rval = b -> GetString ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  FLow::GetBackStr()=%s, this=%s",
	       rval,Id());
    return rval;
}

int Flow::
AllocBack () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    int where = FreeBacks.Pop ();
    if (where == INT_MIN)
    {
	where = NumOfBacks ();
	NumberOfBacks++;
    }
    Background *bb = new Background;
    FlowBack[where] = *bb;
    TR_DEBUG2(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::AllocBack()=%d, this=%s",
	      where, Id());
    return where;
}

void Flow::
DelBack (int which) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    tr_assert (which >= 0);
    if (which < NumOfBacks ())
    {
	Background *bb = FlowBack[which].dref ();
	if (bb != NULL)
	{
	    delete bb;
	    FlowBack[which] = SHMNULL;
	    FreeBacks.Push (which);
	}
    }
    TR_DEBUG1(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  FLow::DelBack(),this=%s",
	      Id());
}

int Flow::
NumOfBacks () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    int rval = NumberOfBacks;
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::NumOfBacks()=%d, this=%s",
	       rval,Id());
    return rval;
}

boolean Flow::
ChangeLoop (const char *expr) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Expr *ee = LoopExpr.dref ();
    tr_assert (ee != NULL);
    boolean rval = ee -> Change (expr);
    TR_DEBUG2(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::ChangeLoop()=%d, this=%x",
	      rval, Id());
    return rval;
}

Expr *Flow::
GetLoopPtr () {
    Expr *rval = LoopExpr.dref ();
    tr_assert (rval != NULL);
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetLoopPtr()=%s: this=%s",
	       rval, Id());
    return rval;
}

const char *Flow::
GetLoopStr () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "ENTER Flow::GetLoopStr(), this = %x,%s",
	       this, to_cmd_line ());
    Expr *ee = LoopExpr.dref ();
    tr_assert (ee != NULL);
    const char *rval = ee -> GetString ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetLoopStr()=%s, this=%s",
	       rval,Id());
    return rval;
}

const char *Flow::
GetLoopVal (ValueDisplayType type) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Expr *ee = LoopExpr.dref ();
    tr_assert (ee != NULL);
    const char *rval = ee -> GetValue (type);
    TR_DEBUG2(FLOW_SUPPORT, BIT_30, FALSE,
	      "EXIT  Flow::GetLoopVal()=%s, this=%s",
	      rval,Id());
    return rval;
}

void Flow::
HaltLoop () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);
    Expr *ee = LoopExpr.dref ();
    tr_assert (ee != NULL);
    ee -> Jam (0.0);
    TR_DEBUG1 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::HaltLoop(),this=%s",
	       Id());
}

void Flow::
SetNotifyOnTest (boolean test) {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    NotifyOnTest = test;
    TR_DEBUG1(FLOW_SUPPORT, BIT_30, FALSE,
	      "ENTER Flow::SetNotifyOnTest(), this=%s",
	      Id());
}

boolean Flow::
GetLoopVal () {
    tr_check_shm_sem_c CheckShmSem(__FILE__,__LINE__);

    Expr *ee = LoopExpr.dref ();
    tr_assert (ee != NULL);
    boolean rval = (boolean) ee -> GetValue ();
    TR_DEBUG2 (FLOW_SUPPORT, BIT_30, FALSE,
	       "EXIT  Flow::GetLoopVal()=%d, this=%s",
	       rval,Id());
    return rval;
}

void Flow::
SetAncChange__vtl(BaseStatusObj *,
		  const EVO_Status &status,
		  EVO_StatusData *data) {
			// Ancestor change actions are: 1) (DeletedSubObj)
			// dependent object getting deleted: just update
			// references from old object to new (NULL) object 2)
			// (SubObjChange) dependent object coming back from NULL
			// to actual: just update references from old object
			// (NULL)to new (NULL) object At this time C++ does not
			// allow the following (08/04/93):
			//
			// if ((status.IsChangeBitSet(EVO_Status::DeletedSubObj)) ||
                        //     (status.IsChangeBitSet(EVO_Status::ChangedSubObj)))
			// therefore, the following code:
    boolean update_pointer = FALSE;
    if (status.IsChangeBitSet(EVO_Status::DeletedSubObj))
	update_pointer = TRUE;
    else if (status.IsChangeBitSet(EVO_Status::ChangedSubObj))
	update_pointer = TRUE;
    if (update_pointer == TRUE) {
	if (data != NULL)
	{
	    if (data->GetType() == EVO_StatusData::SubObjChange)
	    {
		EVO_SubObjChangeData *real_data	= (EVO_SubObjChangeData *)data;
		const BaseSymbol *old_symbol	= real_data->GetOldSymbol();
			// This is the sub-object that changed
		const BaseSymbol *new_symbol = real_data->GetNewSymbol();
			// any reference to old_symbol should be changed to
			// reference new_symbol 
		EV_ASSERT0(old_symbol != NULL, "NULL symbol");
		EV_ASSERT0(new_symbol != NULL, "NULL symbol");

		for (int ii = 0; ii < NumberOfNodes; ii++)
		{
		    ev_BaseNode *bn = NodeTable[ii].dref();
		    if ((bn != NULL) && (bn->GetType() == NODE_FLOW))
		    {
			BaseSymbol *exec = ((FlowNode *)bn)->GetExecPtr();
			if ((exec != NULL) && (exec == old_symbol))
			    ((FlowNode *)bn)->PutExec((BaseSymbol *)new_symbol);
		    }
		}
	    }
	}
    }
}

void Flow::
DebugDump__vtl(FILE * file, unsigned int indent, const char*)
const {
    BaseSymbol::DebugDump__vtl (file,indent);
    Flow * const Tmp = (Flow * const)this;
    fprintf (file, "%sFlow %s\n", Indent(indent), GetName ());

    fprintf (file, "%sLoopExpr = %s\n",Indent(indent+1), LoopExpr->Id());
    fprintf (file, "%sWhichEntry = %d\n", Indent(indent+1), WhichEntry);
    fprintf (file, "%sNumOfExecs = %d\n", Indent(indent+1), NumOfExecs);
    fprintf (file, "%sExitedPort = %d\n", Indent(indent+1), ExitedPort);
    fprintf (file, "%sRunNumber = %d\n", Indent(indent+1), RunNumber);

    fprintf(file, "%sEnableTimers=%s\tCumulativeTimer=%s (%s)\n",
	    Indent(indent+1),
	    EnableTimers.Id(),
	    Tmp->CumulativeTimer.text(),
	    CumulativeTime.Id() );
    fprintf(file, "%sLastExecTimer=%s (%s)\n",
	    Indent(indent+1),
	    Tmp->LastExecTimer.text(),
	    LastExecTime.Id() );

    fprintf (file,
	     "%sStandardCalOnly = %s\n",
	     Indent(indent+1), StandardCalOnly->Id());
    fprintf (file,
	     "%sUserDefaultCalId = %d\n",
	     Indent(indent+1), UserDefaultCalId);
    fprintf (file,
	     "%sCalFile = %s\n",
	     Indent(indent+1), (char*)CalFile);

    fprintf (file,
	     "%sNodeObjects:\n",
	     Indent(indent+1));

    int size = NodeTable.GetSize ();
    for (int ii = 0; ii < size; ii++)
    {
	ev_BaseNode *pp = NodeTable[ii].dref ();
	if (pp != NULL)
	    pp -> DebugDump (file, indent + 2);
    }

    fprintf (file, "%sEntryPoints:\n", Indent(indent+1));
    size = EntryPoint.GetSize ();
    for (ii = 0; ii < size; ii++)
    {
	ev_BaseNode *pp = EntryPoint[ii].dref ();
	if (pp != NULL)
	{
	    EVResource evr (EV_FLOW);
	    fprintf (file,
		     "%s%s = NodeObject at (%d,%d)\n",
		     Indent(indent+2),
		     evr.GetResourceLabel (ii),
		     pp -> GetX (),
		     pp -> GetY ());
	}
    }

    fprintf (file,
	     "%sBackground Format: (BACKSTR, int1, int2, ...)\n",
	     Indent(indent+1));
    size = FlowBack.GetSize ();
    for (ii = 0; ii < size; ii++)
    {
	Background *bb = FlowBack[ii].dref ();
	if (bb != NULL)
	{
	    fprintf (file,
		     "%s(\"%s\"",
		     Indent(indent+2), bb -> GetString ());
	    
	    int size = bb -> NumOfBackInts ();
	    for (int jj = 0; jj < size; jj++)
		fprintf (file, ",%d", bb -> GetInt (jj));
	    fprintf (file, ")\n");
	}
    }
}

void Flow::
ResetCumulativeCounts() {
    tr_check_shm_sem_c ShmSem(__FILE__,__LINE__);

    CumulativeTimer.set(0.0);
    CumulativeTime.Jam( 0.0 );
    LastExecTime.Jam( 0.0 );

    for (int ii = 0; ii < NumberOfNodes; ii++)
    {
	ev_BaseNode *pp = NodeTable[ii].dref();
	if (pp != NULL)
	{
	    pp -> ResetCumulativeTraverseCounts();
	    pp -> ResetCumulativeTimers();
	}
    }
}

boolean Flow::
DoCalibration() {
    tr_check_shm_sem_c ShmSem(__FILE__,__LINE__);

    return boolean(DoCalWhen.DateHasComeDue() || EV_CURTP->CalLoadboard()); 
}

/******************************************************************************/
/*     InitTPCache __vtl                                                      */
/******************************************************************************/

void Flow::
InitTPCache__vtl(evc_prog_reset_t mode) {
    TR_DEBUG1(TESTPROG_CACHE,BIT_0,FALSE,"Enter Flow::InitTPCache__vtl(), this = %s", Id());

    int NumNodes = GetNodeTableSize ();
    for (int node_num = 0; node_num < NumNodes; node_num++)
    {
        FlowNode *fn = (FlowNode *) GetNode (node_num);
        if ((fn != NULL) && (fn -> GetType () == PORT_FLOW))
	{
            EV_Mcl *Mcl = &(fn -> GetEntryMcl ());
            Mcl -> SetNeedsEval ();
            Mcl = &(fn -> GetExitMcl ());
            Mcl -> SetNeedsEval ();
        }
    }
    TR_DEBUG1(TESTPROG_CACHE,BIT_0,FALSE,
	      "Exit Flow::InitTPCache__vtl(), this = %s",
	      Id());
}

void Flow::
SetUserDefaultCal(int cal_id) {
    TR_DEBUG1(TIMING_CAL, BIT_25, FALSE,
	      "Flow::SetUserDefaultCal to %3d",
              cal_id);
    UserDefaultCalId = cal_id;
}

int Flow::
GetUserDefaultCal () {
    TR_DEBUG1(TIMING_CAL, BIT_25, FALSE,
              "Flow::GetUserDefaultCal return %3d",
	      UserDefaultCalId);
    return UserDefaultCalId;
}


void Flow::
SetStandardCalOnly(boolean yes) {
    StandardCalOnly.dref()->Change(BooleanName(yes));
}

boolean Flow::
GetStandardCalOnly() {
    return (boolean)StandardCalOnly.dref()->GetValue();
}

Expr* Flow::
GetStandardCalExpr() {
    return StandardCalOnly.dref();
}

#if PHASE_EXEC_LIB
boolean Flow::
ExecEntryPoint(ev_flow_entry_point new_entry_point) {
    ev_flow_entry_point old_entry_point	= GetCurrentEntryPoint();
    SetEntryPoint(new_entry_point);
    boolean rval = Exec();
    SetEntryPoint(old_entry_point);
    return rval;
}
#endif

const char*
Name(evf_flow_entry_point entry_point) {
    switch(entry_point)
    {
      case EVF_NOT_ENTRY_POINT: return "EVF_NOT_ENTRY_POINT";
      case EVF_ON_START: return "EVF_ON_START";
      case EVF_ON_RESTART: return "EVF_ON_RESTART";
      case EVF_ON_LOAD: return "EVF_ON_LOAD";
      case EVF_ON_UNLOAD: return "EVF_ON_UNLOAD";
      case EVF_ON_RESET: return "EVF_ON_RESET";
      case EVF_ON_HALT: return "EVF_ON_HALT";
      case EVF_ON_POWERDOWN: return "EVF_ON_POWERDOWN";
      case EVF_ON_BEGIN_LOT: return "EVF_ON_BEGIN_LOT";
      case EVF_ON_END_LOT: return "EVF_ON_END_LOT";
      case EVF_ON_DEBUG: return "EVF_ON_DEBUG";
      case EVF_ON_GPIB_SRQ: return "EVF_ON_GPIB_SRQ";
      case EVF_ON_INIT_FLOW: return "EVF_ON_INIT_FLOW";
      case EVF_ON_AFTER_BIN: return "EVF_ON_AFTER_BIN";
      case EVF_START_OF_WAFER: return "EVF_START_OF_WAFER";
      case EVF_END_OF_WAFER: return "EVF_END_OF_WAFER";
      case EVF_BIN_OVERFLOW: return "EVF_BIN_OVERFLOW";
      case EVF_USR_DEF_0: return "EVF_USR_DEF_0";
      case EVF_USR_DEF_1: return "EVF_USR_DEF_1";
      case EVF_USR_DEF_2: return "EVF_USR_DEF_2";
      case EVF_USR_DEF_3: return "EVF_USR_DEF_3";
      case EVF_USR_DEF_4: return "EVF_USR_DEF_4";
      case EVF_USR_DEF_5: return "EVF_USR_DEF_5";
      case EVF_USR_DEF_6: return "EVF_USR_DEF_6";
      case EVF_USR_DEF_7: return "EVF_USR_DEF_7";
      case EVF_USR_DEF_8: return "EVF_USR_DEF_8";
      case EVF_USR_DEF_9: return "EVF_USR_DEF_9";
          
    } /* switch */
    return "Unknown evf_flow_entry_point";
    // Here rather then switch/default to silence compiler warning.
}			/* Name */


// Here it is. To get the problem go down to the bottom of the file, and
// then up a few lines such that you're inside the switch statement
// block at the last case. Then go to the end of the line and hit a
// carriage return. Then, take a coffee break :-) The carriage return
// inserts very quickly on 4.29, but slooowwwwly on 4.30.
