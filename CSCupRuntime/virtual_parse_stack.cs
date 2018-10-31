
using CSCupRuntime;

namespace TUVienna.CS_CUP.Runtime
{

	using System.Collections.Generic;

	/** This class implements a temporary or "virtual" parse stack that 
	 *  replaces the top portion of the actual parse stack (the part that 
	 *  has been changed by some Set of operations) while maintaining its
	 *  original contents.  This data structure is used when the parse needs 
	 *  to "parse ahead" to determine if a given error recovery attempt will 
	 *  allow the parse to continue far enough to consider it successful.  Once 
	 *  success or failure of parse ahead is determined the system then 
	 *  reverts to the original parse stack (which has not actually been 
	 *  modified).  Since parse ahead does not execute actions, only parse
	 *  state is maintained on the virtual stack, not full Symbol objects.
	 *
	 * @see     java_cup.runtime.lr_parser
	 * @version last updated: 7/3/96
	 * @author  Frank Flannery
     * translated to C# 08.09.2003 by Samuel Imriska
	 */

	public class virtual_parse_stack 
	{
		/*-----------------------------------------------------------*/
		/*--- Constructor(s) ----------------------------------------*/
		/*-----------------------------------------------------------*/

		/** Constructor to build a virtual stack out of a real stack. */
		public virtual_parse_stack(StackList<Symbol> shadowing_stack) 
														  {
															  /* sanity check */
															  if (shadowing_stack == null)
		throw new System.Exception(
				  "Internal parser error: attempt to create null virtual stack");

		/* Set up our internals */
		real_stack = shadowing_stack;
		vstack     = new Stack<int>();
		real_next  = 0;

		/* get one element onto the virtual portion of the stack */
		get_from_real();
	}

	/*-----------------------------------------------------------*/
	/*--- (Access to) Instance Variables ------------------------*/
	/*-----------------------------------------------------------*/
       
	/** The real stack that we shadow.  This is accessed when we move off
	 *  the bottom of the virtual portion of the stack, but is always left
	 *  unmodified.
	 */
	protected StackList<Symbol> real_stack;

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Top of stack indicator for where we leave off in the real stack.
	 *  This is measured from top of stack, so 0 would indicate that no
	 *  elements have been "moved" from the real to virtual stack. 
	 */
	protected int real_next;

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** The virtual top portion of the stack.  This stack contains Integer
	 *  objects with state numbers.  This stack shadows the top portion
	 *  of the real stack within the area that has been modified (via operations
	 *  on the virtual stack).  When this portion of the stack becomes empty we 
	 *  transfer elements from the underlying stack onto this stack. 
	 */
	protected Stack<int> vstack;

	/*-----------------------------------------------------------*/
	/*--- General Methods ---------------------------------------*/
	/*-----------------------------------------------------------*/

	/** Transfer an element from the real to the virtual stack.  This assumes 
	 *  that the virtual stack is currently empty.  
	 */
	protected void get_from_real()
{
	Symbol stack_sym;

	/* don't transfer if the real stack is empty */
	if (real_next >= real_stack.Count) return;

    /* get a copy of the first Symbol we have not transfered */
    //CSCupRuntime: Error Recovery mechanism has a Bug, it does not work.  #926
    //https://github.com/TypeCobolTeam/TypeCobol/issues/926
    //stack_sym = (Symbol)real_stack.ToArray()[real_stack.Count-1-real_next]; 
    stack_sym = real_stack.ElementAtFromTop(real_next);

            /* record the transfer */
            real_next++;

	/* put the state number from the Symbol onto the virtual stack */
	vstack.Push(stack_sym.parse_state);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Indicate whether the stack is empty. */
	public bool empty()
{
	/* if vstack is empty then we were unable to transfer onto it and 
	 the whole thing is empty. */
	return (vstack.Count==0);
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
      
	/** Return value on the top of the stack (without popping it). */
	public int top()
{
	if (vstack.Count==0)
	throw new System.Exception(
	"Internal parser error: top() called on empty virtual stack");

	return vstack.Peek();
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Pop the stack. */
	public void pop() 
{
	if (vstack.Count==0)
	throw new System.Exception(
	"Internal parser error: pop from empty virtual stack");

	/* pop it */
	vstack.Pop();

	/* if we are now empty transfer an element (if there is one) */
	if (vstack.Count==0)
	get_from_real();
}

	/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

	/** Push a state number onto the stack. */
	public void push(int state_num)
{
	vstack.Push(state_num);
}

	/*-----------------------------------------------------------*/

}
}