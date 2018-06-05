namespace TUVienna.CS_CUP
{
using System;
using System.IO;
using System.Collections;

/** 
 * This class handles emitting generated code for the resulting parser.
 * The various parse tables must be constructed, etc. before calling any 
 * routines in this class.<p>  
 *
 * Three classes are produced by this code:
 *   <dl>
 *   <dt> symbol constant class
 *   <dd>   this contains constant declarations for each terminal (and 
 *          optionally each non-terminal).
 *   <dt> action class
 *   <dd>   this non-public class contains code to invoke all the user actions 
 *          that were embedded in the parser specification.
 *   <dt> parser class
 *   <dd>   the specialized parser class consisting primarily of some user 
 *          supplied general and initialization code, and the parse tables.
 *   </dl><p>
 *
 *  Three parse tables are created as part of the parser class:
 *    <dl>
 *    <dt> production table
 *    <dd>   lists the LHS non terminal number, and the length of the RHS of 
 *           each production.
 *    <dt> action table
 *    <dd>   for each state of the parse machine, gives the action to be taken
 *           (shift, reduce, or error) under each lookahead symbol.<br>
 *    <dt> reduce-goto table
 *    <dd>   when a reduce on a given production is taken, the parse stack is 
 *           popped back a number of elements corresponding to the RHS of the 
 *           production.  This reveals a prior state, which we transition out 
 *           of under the LHS non terminal symbol for the production (as if we
 *           had seen the LHS symbol rather than all the symbols matching the 
 *           RHS).  This table is indexed by non terminal numbers and indicates 
 *           how to make these transitions. 
 *    </dl><p>
 * 
 * In addition to the method interface, this class maintains a series of 
 * public global variables and flags indicating how misc. parts of the code 
 * and other output is to be produced, and counting things such as number of 
 * conflicts detected (see the source code and public variables below for
 * more details).<p> 
 *
 * This class is "static" (contains only static data and methods).<p> 
 *
 * @see TUVienna.CS_CUP.main
 * @version last update: 11/25/95
 * @author Scott Hudson
 * translated to C# 08.09.2003 by Samuel Imriska
 */

/* Major externally callable routines here include:
     symbols               - emit the symbol constant class 
     parser                - emit the parser class

   In addition the following major internal routines are provided:
     emit_package          - emit a package declaration
     emit_action_code      - emit the class containing the user's actions 
     emit_production_table - emit declaration and init for the production table
     do_action_table       - emit declaration and init for the action table
     do_reduce_table       - emit declaration and init for the reduce-goto table

   Finally, this class uses a number of public instance variables to communicate
   optional parameters and flags used to control how code is generated,
   as well as to report counts of various things (such as number of conflicts
   detected).  These include:

   prefix                  - a prefix string used to prefix names that would 
			     otherwise "pollute" someone else's name space.
   package_name            - name of the package emitted code is placed in 
			     (or null for an unnamed package.
   symbol_const_class_name - name of the class containing symbol constants.
   parser_class_name       - name of the class for the resulting parser.
   action_code             - user supplied declarations and other code to be 
			     placed in action class.
   parser_code             - user supplied declarations and other code to be 
			     placed in parser class.
   init_code               - user supplied code to be executed as the parser 
			     is being initialized.
   scan_code               - user supplied code to get the next Symbol.
   start_production        - the start production for the grammar.
   import_list             - list of imports for use with action class.
   num_conflicts           - number of conflicts detected. 
   nowarn                  - true if we are not to issue warning messages.
   not_reduced             - count of number of productions that never reduce.
   unused_term             - count of unused terminal symbols.
   unused_non_term         - count of unused non terminal symbols.
   *_time                  - a series of symbols indicating how long various
			     sub-parts of code generation took (used to produce
			     optional time reports in main).
*/

public class emit {

  /*-----------------------------------------------------------*/
  /*--- Constructor(s) ----------------------------------------*/
  /*-----------------------------------------------------------*/

  /** Only constructor is private so no instances can be created. */
  private emit() { }

  /*-----------------------------------------------------------*/
  /*--- Static (Class) Variables ------------------------------*/
  /*-----------------------------------------------------------*/

  /** The prefix placed on names that pollute someone else's name space. */
  public static string prefix = "CUP_";

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Package that the resulting code goes into (null is used for unnamed). */
  public static string package_name = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Name of the generated class for symbol constants. */
  public static string symbol_const_class_name = "sym";

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Name of the generated parser class. */
  public static string parser_class_name = "parser";

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** User declarations for direct inclusion in user action class. */
  public static string action_code = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** User declarations for direct inclusion in parser class. */
  public static string parser_code = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** User code for user_init() which is called during parser initialization. */
  public static string init_code = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** User code for scan() which is called to get the next Symbol. */
  public static string scan_code = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** The start production of the grammar. */
  public static production start_production = null;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** List of imports (strings containing class names) to go with actions. */
  public static Stack import_list = new Stack();

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Number of conflict found while building tables. */
  public static int num_conflicts = 0;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Do we skip warnings? */
  public static bool nowarn = false;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Count of the number on non-reduced productions found. */
  public static int not_reduced = 0;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Count of unused terminals. */
  public static int unused_term = 0;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Count of unused non terminals. */
  public static int unused_non_term = 0;

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /* Timing values used to produce timing report in main.*/

  /** Time to produce symbol constant class. */
  public static long symbols_time          = 0;

  /** Time to produce parser class. */
  public static long parser_time           = 0;

  /** Time to produce action code class. */
  public static long action_code_time      = 0;

  /** Time to produce the production table. */
  public static long production_table_time = 0;

  /** Time to produce the action table. */
  public static long action_table_time     = 0;

  /** Time to produce the reduce-goto table. */
  public static long goto_table_time       = 0;

  /* frankf 6/18/96 */
  protected static bool _lr_values;

  /** whether or not to emit code for left and right values */
  public static bool lr_values() {return _lr_values;}
  public static void set_lr_values(bool b) { _lr_values = b;}

  /*-----------------------------------------------------------*/
  /*--- General Methods ---------------------------------------*/
  /*-----------------------------------------------------------*/

  /** Build a string with the standard prefix. 
   * @param str string to prefix.
   */
  public static string pre(string str) {
    return prefix + parser_class_name + "_" + str;
  }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit a package spec if the user wants one. 
   * @param out stream to produce output on.
   */
  protected static void emit_package(TextWriter cout)
    {
      /* generate a package spec if we have a name for one */
      if (package_name != null) {
	cout.WriteLine("namespace " + package_name + "\n {"); cout.WriteLine();
      }
    }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit code for the symbol constant class, optionally including non terms,
   *  if they have been requested.  
   * @param out            stream to produce output on.
   * @param emit_non_terms do we emit constants for non terminals?
   * @param sym_interface  should we emit an interface, rather than a class?
   */
  public static void symbols(TextWriter cout, 
			     bool emit_non_terms, bool sym_interface)
    {
      terminal term;
      non_terminal nt;
      string class_or_interface = (sym_interface)?"interface":"class";

      long start_time = DateTime.Now.Ticks;

      /* top of file */
      cout.WriteLine();
      cout.WriteLine("//----------------------------------------------------"); 
      cout.WriteLine("// The following code was generated by " + 
							   version.title_str);
      cout.WriteLine("// " + DateTime.Now);
      cout.WriteLine("//----------------------------------------------------"); 
      cout.WriteLine();
      emit_package(cout);

      /* class header */
      cout.WriteLine("/** CUP generated " + class_or_interface + 
		  " containing symbol constants. */");
      cout.WriteLine("public " + class_or_interface + " " + 
		  symbol_const_class_name + " {");

      cout.WriteLine("  /* terminals */");

      /* walk over the terminals */              /* later might sort these */
	  IEnumerator e = terminal.all();
      while ( e.MoveNext() )
	{
	  term = (terminal)e.Current;

	  /* output a constant decl for the terminal */
	  cout.WriteLine("  public const int " + term.name() + " = " + 
		      term.index() + ";");
	}

      /* do the non terminals if they want them (parser doesn't need them) */
      if (emit_non_terms)
	{
          cout.WriteLine();
          cout.WriteLine("  /* non terminals */");

          /* walk over the non terminals */       /* later might sort these */
		  IEnumerator e1 = non_terminal.all();
          while ( e1.MoveNext() )
	    {
	      nt = (non_terminal)e1.Current;
    
	      /* output a constant decl for the terminal */
	      cout.WriteLine("  const int " + nt.name() + " = " + 
		          nt.index() + ";");
	    }
	}

      /* end of class */
      cout.WriteLine("\t}");
      if (package_name!=null)
           cout.WriteLine("}");

      symbols_time = DateTime.Now.Ticks - start_time;
    }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit code for the non-public class holding the actual action code. 
   * @param out        stream to produce output on.
   * @param start_prod the start production of the grammar.
   */
  protected static void emit_action_code(TextWriter cout, production start_prod)
    {
      production prod;

      long start_time = DateTime.Now.Ticks;

      /* class header */
      cout.WriteLine();
      cout.WriteLine(
       "/** Cup generated class to encapsulate user supplied action code.*/"
      );  
      cout.WriteLine("public class " +  pre("actions") + " {");

      /* user supplied code */
      if (action_code != null)
	{
	  cout.WriteLine();
          cout.WriteLine(action_code);
	}

      /* field for parser object */
      cout.WriteLine("  private "+ parser_class_name+" my_parser;");

      /* constructor */
      cout.WriteLine();
      cout.WriteLine("  /** Constructor */");
      cout.WriteLine("  public " + pre("actions") + "("+ parser_class_name+" t_parser) {");
      cout.WriteLine("    this.my_parser = t_parser;");
      cout.WriteLine("  }");

      /* action method head */
      cout.WriteLine();
      cout.WriteLine("  /** Method with the actual generated action code. */");
      cout.WriteLine("  public   TUVienna.CS_CUP.Runtime.Symbol " + 
		     pre("do_action") + "(");
      cout.WriteLine("    int                        " + pre("act_num,"));
      cout.WriteLine("    TUVienna.CS_CUP.Runtime.lr_parser " + pre("parser,"));
      cout.WriteLine("    System.Collections.Stack            xstack1,");
      cout.WriteLine("    int                        " + pre("top)"));
    //  cout.WriteLine("    throws java.lang.Exception");
      cout.WriteLine("    {");

      /* declaration of result symbol */
      /* New declaration!! now return Symbol
	 6/13/96 frankf */
      cout.WriteLine("      /* Symbol object for return from actions */");
      cout.WriteLine("      mStack "+pre("stack")+" =new mStack(xstack1);");
      cout.WriteLine("      TUVienna.CS_CUP.Runtime.Symbol " + pre("result") + ";");
      cout.WriteLine();

      /* switch top */
      cout.WriteLine("      /* select the action based on the action number */");
      cout.WriteLine("      switch (" + pre("act_num") + ")");
      cout.WriteLine("        {");

      /* emit action code for each production as a separate case */
	  IEnumerator p = production.all();
      while ( p.MoveNext())
	{
	  prod = (production)p.Current;

	  /* case label */
          cout.WriteLine("          /*. . . . . . . . . . . . . . . . . . . .*/");
          cout.WriteLine("          case " + prod.index() + ": // " + 
					  prod.to_simple_string());

	  /* give them their own block to work in */
	  cout.WriteLine("            {");

	  /* create the result symbol */
	  /*make the variable RESULT which will point to the new Symbol (see below)
	    and be changed by action code
	    6/13/96 frankf */
	  cout.WriteLine("              " +  prod.lhs().the_symbol().stack_type() +
		      " RESULT = null;");

	  /* Add code to propagate RESULT assignments that occur in
	   * action code embedded in a production (ie, non-rightmost
	   * action code). 24-Mar-1998 CSA
	   */
	  for (int i=0; i<prod.rhs_length(); i++) {
	    // only interested in non-terminal symbols.
	    if (prod.rhs(i).GetType()!= typeof(symbol_part)) continue;
	    symbol s = ((symbol_part)prod.rhs(i)).the_symbol();
	    if (s.GetType()!=typeof(non_terminal)) continue;
	    // skip this non-terminal unless it corresponds to
	    // an embedded action production.
	    if (((non_terminal)s).is_embedded_action == false) continue;
	    // OK, it fits.  Make a conditional assignment to RESULT.
	    int index = prod.rhs_length() - i - 1; // last rhs is on top.
	    cout.WriteLine("              " + "// propagate RESULT from " +
			s.name());
	    cout.WriteLine("              " + "if ( " +
	      "((TUVienna.CS_CUP.Runtime.Symbol) " + emit.pre("stack") + ".elementAt("
              + emit.pre("top") + "-" + index + ")).value != null )");
	    cout.WriteLine("                " + "RESULT = " +
	      "(" + prod.lhs().the_symbol().stack_type() + ") " +
	      "((TUVienna.CS_CUP.Runtime.Symbol) " + emit.pre("stack") + ".elementAt("
              + emit.pre("top") + "-" + index + ")).value;");
	  }

        /* if there is an action string, emit it */
          if (prod.action() != null && prod.action().code_string() != null &&
              !prod.action().Equals(""))
            cout.WriteLine(prod.action().code_string());

	  /* here we have the left and right values being propagated.  
		must make this a command line option.
	     frankf 6/18/96 */

         /* Create the code that assigns the left and right values of
            the new Symbol that the production is reducing to */
	  if (emit.lr_values()) {	    
	    int loffset;
	    string leftstring, rightstring;
	    int roffset = 0;
	    rightstring = "((TUVienna.CS_CUP.Runtime.Symbol)" + emit.pre("stack") + ".elementAt(" + 
	      emit.pre("top") + "-" + roffset + ")).right";	  
	    if (prod.rhs_length() == 0) 
	      leftstring = rightstring;
	    else {
	      loffset = prod.rhs_length() - 1;
	      leftstring = "((TUVienna.CS_CUP.Runtime.Symbol)" + emit.pre("stack") + ".elementAt(" + 
		emit.pre("top") + "-" + loffset + ")).left";	  
	    }
	    cout.WriteLine("              " + pre("result") + " = new TUVienna.CS_CUP.Runtime.Symbol(" + 
			prod.lhs().the_symbol().index() + "/*" +
			prod.lhs().the_symbol().name() + "*/" + 
			", " + leftstring + ", " + rightstring + ", RESULT);");
	  } else {
	    cout.WriteLine("              " + pre("result") + " = new TUVienna.CS_CUP.Runtime.Symbol(" + 
			prod.lhs().the_symbol().index() + "/*" +
			prod.lhs().the_symbol().name() + "*/" + 
			", RESULT);");
	  }
	  
	  /* end of their block */
	  cout.WriteLine("            }");

	  /* if this was the start production, do action for accept */
	  if (prod == start_prod)
	    {
	      cout.WriteLine("          /* ACCEPT */");
	      cout.WriteLine("          " + pre("parser") + ".done_parsing();");
	    }

	  /* code to return lhs symbol */
	  cout.WriteLine("          return " + pre("result") + ";");
	  cout.WriteLine();
	}

      /* end of switch */
      cout.WriteLine("          /* . . . . . .*/");
      cout.WriteLine("          default:");
      cout.WriteLine("            throw new System.Exception(");
      cout.WriteLine("               \"Invalid action number found in " +
				  "internal parse table\");");
      cout.WriteLine();
      cout.WriteLine("        }");

      /* end of method */
      cout.WriteLine("    }");

      /* end of class */
      cout.WriteLine("}");
      cout.WriteLine();
      action_code_time = DateTime.Now.Ticks - start_time;
    }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit the production table. 
   * @param out stream to produce output on.
   */
  protected static void emit_production_table(TextWriter cout)
    {
      production[] all_prods;
      production prod;
      long start_time = DateTime.Now.Ticks;

      /* collect up the productions in order */
      all_prods = new production[production.number()];
	  IEnumerator p = production.all();
      while ( p.MoveNext() )
	{
	  prod = (production)p.Current;
	  all_prods[prod.index()] = prod;
	}

	  short[][] prod_table=new short[production.number()][];
	  for (int i=0;i<production.number();i++)
	  {
		  prod_table[i]= new short[2];
	  }
      for (int i = 0; i<production.number(); i++)
	{
	  prod = all_prods[i];
	  // { lhs symbol , rhs size }
	  prod_table[i][0] = (short) prod.lhs().the_symbol().index();
	  prod_table[i][1] = (short) prod.rhs_length();
	}
      /* do the top of the table */
      cout.WriteLine();
      cout.WriteLine("  /** Production table. */");
      cout.WriteLine("  protected static readonly short[][] _production_table = ");
#if JAVA_CUP_WAY
      cout.Write  ("    unpackFromStrings(");
      do_table_as_string(cout, prod_table);
      cout.WriteLine(");");
#else
        //JCM: Parsing table Optimization: 
        //See : https://github.com/TypeCobolTeam/TypeCobol/issues/897
        do_table_as_table(cout, prod_table);
#endif

            /* do the public accessor method */
            cout.WriteLine();
      cout.WriteLine("  /** Access to production table. */");
      cout.WriteLine("  public override short[][] production_table() " + 
						 "{return _production_table;}");

      production_table_time = DateTime.Now.Ticks - start_time;
    }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit the action table. 
   * @param out             stream to produce output on.
   * @param act_tab         the internal representation of the action table.
   * @param compact_reduces do we use the most frequent reduce as default?
   */

  protected static void do_action_table(
    TextWriter        cout, 
    parse_action_table act_tab,
    bool            compact_reduces)

    {
      parse_action_row row;
      parse_action     act;
      int              red;

      long start_time = DateTime.Now.Ticks;

      /* collect values for the action table */
      short[][] action_table = new short[act_tab.num_states()][];
      /* do each state (row) of the action table */
      for (int i = 0; i < act_tab.num_states(); i++)
	{
	  /* get the row */
	  row = act_tab.under_state[i];

	  /* determine the default for the row */
	  if (compact_reduces)
	    row.compute_default();
	  else
	    row.default_reduce = -1;

	  /* make temporary table for the row. */
	  short[] temp_table = new short[2*parse_action_row.size()];
	  int nentries = 0;

	  /* do each column */
	  for (int j = 0; j < parse_action_row.size(); j++)
	    {
	      /* extract the action from the table */
	      act = row.under_term[j];

	      /* skip error entries these are all defaulted out */
	      if (act.kind() != parse_action.ERROR)
		{
		  /* first put in the symbol index, then the actual entry */

		  /* shifts get positive entries of state number + 1 */
		  if (act.kind() == parse_action.SHIFT)
		    {
		      /* make entry */
		      temp_table[nentries++] = (short) j;
		      temp_table[nentries++] = (short)
			(((shift_action)act).shift_to().index() + 1);
		    }

		  /* reduce actions get negated entries of production# + 1 */
		  else if (act.kind() == parse_action.REDUCE)
		    {
		      /* if its the default entry let it get defaulted out */
		      red = ((reduce_action)act).reduce_with().index();
		      if (red != row.default_reduce) {
			/* make entry */
			temp_table[nentries++] = (short) j;
			temp_table[nentries++] = (short) (-(red+1));
		      }
		    } else if (act.kind() == parse_action.NONASSOC)
		      {
			/* do nothing, since we just want a syntax error */
		      }
		  /* shouldn't be anything else */
		  else
		    throw new internal_error("Unrecognized action code " + 
					     act.kind() + " found in parse table");
		}
	    }

	  /* now we know how big to make the row */
	  action_table[i] = new short[nentries + 2];

	  //System.arraycopy(temp_table, 0, action_table[i], 0, nentries);
		Array.Copy(temp_table,action_table[i],nentries);

	  /* finish off the row with a default entry */
	  action_table[i][nentries++] = -1;
	  if (row.default_reduce != -1)
	    action_table[i][nentries++] = (short) (-(row.default_reduce+1));
	  else
	    action_table[i][nentries++] = 0;
	}

      /* finish off the init of the table */
      cout.WriteLine();
      cout.WriteLine("  /** Parse-action table. */");
      cout.WriteLine("  protected static readonly short[][] _action_table = ");
#if JAVA_CUP_WAY
      cout.Write  ("    unpackFromStrings(");
      do_table_as_string(cout, action_table);
      cout.WriteLine(");");
#else
    //JCM: Parsing table Optimization: 
    //See : https://github.com/TypeCobolTeam/TypeCobol/issues/897
    do_table_as_table(cout, action_table);
#endif

            /* do the public accessor method */
            cout.WriteLine();
      cout.WriteLine("  /** Access to parse-action table. */");
      cout.WriteLine("  public override short[][] action_table() {return _action_table;}");

      action_table_time = DateTime.Now.Ticks - start_time;
    }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit the reduce-goto table. 
   * @param out     stream to produce output on.
   * @param red_tab the internal representation of the reduce-goto table.
   */
  protected static void do_reduce_table(
    TextWriter cout, 
    parse_reduce_table red_tab)
    {
      lalr_state       goto_st;

      long start_time = DateTime.Now.Ticks;

      /* collect values for reduce-goto table */
      short[][] reduce_goto_table = new short[red_tab.num_states()][];
      /* do each row of the reduce-goto table */
      for (int i=0; i<red_tab.num_states(); i++)
	{
	  /* make temporary table for the row. */
	  	  short[] temp_table = new short[2*parse_reduce_row.size()];
	  int nentries = 0;
	  /* do each entry in the row */
	  for (int j=0; j<parse_reduce_row.size(); j++)
	    {
	      /* get the entry */
	      goto_st = red_tab.under_state[i].under_non_term[j];

	      /* if we have none, skip it */
	      if (goto_st != null)
		{
		  /* make entries for the index and the value */
		  temp_table[nentries++] = (short) j;
		  temp_table[nentries++] = (short) goto_st.index();
		}
	    }
	  /* now we know how big to make the row. */
	  reduce_goto_table[i] = new short[nentries+2];
	  Array.Copy(temp_table, reduce_goto_table[i], nentries);

	  /* end row with default value */
	  reduce_goto_table[i][nentries++] = -1;
	  reduce_goto_table[i][nentries++] = -1;
	}

      /* emit the table. */
      cout.WriteLine();
      cout.WriteLine("  /** <code>reduce_goto</code> table. */");
      cout.WriteLine("  protected static readonly short[][] _reduce_table = ");
#if JAVA_CUP_WAY
      cout.Write  ("    unpackFromStrings(");
      do_table_as_string(cout, reduce_goto_table);
      cout.WriteLine(");");
#else
            //JCM: Parsing table Optimization: 
            //See : https://github.com/TypeCobolTeam/TypeCobol/issues/897
            do_table_as_table(cout, reduce_goto_table);
#endif

            /* do the public accessor method */
            cout.WriteLine();
      cout.WriteLine("  /** Access to <code>reduce_goto</code> table. */");
      cout.WriteLine("  public override short[][] reduce_table() {return _reduce_table;}");
      cout.WriteLine();

      goto_table_time = DateTime.Now.Ticks - start_time;
    }

  // print a string array encoding the given short[][] array.
  protected static void do_table_as_string(TextWriter cout, short[][] sa) {
    cout.WriteLine("new string[] {");
    cout.Write("    \"");
    int nchar=0, nbytes=0;
    nbytes+=do_escaped(cout, (char)(sa.Length>>16));
    nchar  =do_newline(cout, nchar, nbytes);
    nbytes+=do_escaped(cout, (char)(sa.Length&0xFFFF));
    nchar  =do_newline(cout, nchar, nbytes);
    for (int i=0; i<sa.Length; i++) {
	nbytes+=do_escaped(cout, (char)(sa[i].Length>>16));
	nchar  =do_newline(cout, nchar, nbytes);
	nbytes+=do_escaped(cout, (char)(sa[i].Length&0xFFFF));
	nchar  =do_newline(cout, nchar, nbytes);
	for (int j=0; j<sa[i].Length; j++) {
	  // contents of string are (value+2) to allow for common -1, 0 cases
	  // (UTF-8 encoding is most efficient for 0<c<0x80)
	  nbytes+=do_escaped(cout, (char)(2+sa[i][j]));
	  nchar  =do_newline(cout, nchar, nbytes);
	}
    }
    cout.Write("\" }");
  }

        /// <summary>
        /// Genetate the array as a C# short[][] array declaration without using
        /// the encoding of the static method do_table_as_string:
        /// With C# static data are not limited to 65535 bytes like in Java.
        /// See : https://github.com/TypeCobolTeam/TypeCobol/issues/897
        /// </summary>
        /// <param name="cout"></param>
        /// <param name="sa"></param>
        protected static void do_table_as_table(TextWriter cout, short[][] sa)
    {
        cout.WriteLine(string.Format("new short[{0}][] {{", sa.Length));
        for (int i = 0; i < sa.Length; i++)
        {
            cout.Write(string.Format("\tnew short[{0}]{{", sa[i].Length));
            string sep = "";
            for (int j = 0; j < sa[i].Length; j++)
            {
                cout.Write(sep);
                cout.Write(sa[i][j]);
                sep = ",";
            }
            if (i != sa.Length - 1)
            {
                cout.WriteLine("},");
            }
            else
            {
                cout.WriteLine("}");
            }
        }
        cout.WriteLine("};");
    }

        // split string if it is very long; start new line occasionally for neatness
        protected static int do_newline(TextWriter cout, int nchar, int nbytes) {
    if (nbytes > 65500)  { cout.WriteLine("\", "); cout.Write("    \""); }
    else if (nchar > 11) { cout.WriteLine("\" +"); cout.Write("    \""); }
    else return nchar+1;
    return 0;
  }
  // own Escape functionfor ocatl numbers because missing in C#
	protected static string toOctal(char c)
	{
		int ic= c;
		System.Text.StringBuilder tmp= new System.Text.StringBuilder();
		while (ic>0)
		{
			tmp.Insert(0,ic%8);
			ic= ((int)ic/8);
		}
		return tmp.ToString();
	}

// output an escape sequence for the given character code.
  protected static int do_escaped(TextWriter cout, char c) {
    System.Text.StringBuilder escape=new System.Text.StringBuilder();
    if (c <= 0xFF) {
      escape.Append(toOctal(c));
      while(escape.Length < 3) escape.Insert(0, '0');
    } else {
	  escape.AppendFormat("{0:x}",((int)c));
      while(escape.Length < 4) escape.Insert(0, '0');
      escape.Insert(0, 'u');
    }
    // in C# version changed fom \
    escape.Insert(0, '/');
    cout.Write(escape.ToString());

    // return number of bytes this takes up in UTF-8 encoding.
    if (c == 0) return 2;
    if (c >= 0x01 && c <= 0x7F) return 1;
    if (c >= 0x80 && c <= 0x7FF) return 2;
    return 3;
  }

  /*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/

  /** Emit the parser subclass with embedded tables. 
   * @param out             stream to produce output on.
   * @param action_table    internal representation of the action table.
   * @param reduce_table    internal representation of the reduce-goto table.
   * @param start_st        start state of the parse machine.
   * @param start_prod      start production of the grammar.
   * @param compact_reduces do we use most frequent reduce as default?
   * @param suppress_scanner should scanner be suppressed for compatibility?
   */

  public static void parser(
    TextWriter        cout, 
    parse_action_table action_table,
    parse_reduce_table reduce_table,
    int                start_st,
    production         start_prod,
    bool            compact_reduces,
    bool            suppress_scanner)
   
    {
      long start_time = DateTime.Now.Ticks;

      /* top of file */
      cout.WriteLine();
      cout.WriteLine("//----------------------------------------------------"); 
      cout.WriteLine("// The following code was generated by " + 
							version.title_str);
      cout.WriteLine("// " +  DateTime.Now);
      cout.WriteLine("//----------------------------------------------------"); 
      cout.WriteLine();
      emit_package(cout);

      /* user supplied imports */
	  IEnumerator myEnum= import_list.GetEnumerator();
      while (myEnum.MoveNext())
	cout.WriteLine("using " + myEnum.Current.ToString() + ";");

      /* class header */
      cout.WriteLine();
      cout.WriteLine("/** "+version.title_str+" generated parser.");
      cout.WriteLine("  * @version " +  DateTime.Now);
      cout.WriteLine("  */");
      cout.WriteLine("public class " + parser_class_name + 
		  " : TUVienna.CS_CUP.Runtime.lr_parser {");

      /* constructors [CSA/davidm, 24-jul-99] */
      cout.WriteLine();
      cout.WriteLine("  /** Default constructor. */");
      cout.WriteLine("  public " + parser_class_name + "():base() {;}");
      if (!suppress_scanner) {
	  cout.WriteLine();
	  cout.WriteLine("  /** Constructor which sets the default scanner. */");
	  cout.WriteLine("  public " + parser_class_name + 
		      "(TUVienna.CS_CUP.Runtime.Scanner s): base(s) {;}");
      }

      /* emit the various tables */
      emit_production_table(cout);
      do_action_table(cout, action_table, compact_reduces);
      do_reduce_table(cout, reduce_table);

      /* instance of the action encapsulation class */
      cout.WriteLine("  /** Instance of action encapsulation class. */");
      cout.WriteLine("  protected " + pre("actions") + " action_obj;");
      cout.WriteLine();

      /* action object initializer */
      cout.WriteLine("  /** Action encapsulation object initializer. */");
      cout.WriteLine("  protected override void init_actions()");
      cout.WriteLine("    {");
      cout.WriteLine("      action_obj = new " + pre("actions") + "(this);");
      cout.WriteLine("    }");
      cout.WriteLine();

      /* access to action code */
      cout.WriteLine("  /** Invoke a user supplied parse action. */");
      cout.WriteLine("  public override TUVienna.CS_CUP.Runtime.Symbol do_action(");
      cout.WriteLine("    int                        act_num,");
      cout.WriteLine("    TUVienna.CS_CUP.Runtime.lr_parser parser,");
      cout.WriteLine("    System.Collections.Stack            xstack1,");
      cout.WriteLine("    int                        top)");
      cout.WriteLine("  {");
      cout.WriteLine("  mStack CUP_parser_stack= new mStack(xstack1);");
      cout.WriteLine("    /* call code in generated class */");
      cout.WriteLine("    return action_obj." + pre("do_action(") +
                  "act_num, parser, stack, top);");
      cout.WriteLine("  }");
      cout.WriteLine("");


      /* method to tell the parser about the start state */
      cout.WriteLine("  /** Indicates start state. */");
      cout.WriteLine("  public override int start_state() {return " + start_st + ";}");

      /* method to indicate start production */
      cout.WriteLine("  /** Indicates start production. */");
      cout.WriteLine("  public override int start_production() {return " + 
		     start_production.index() + ";}");
      cout.WriteLine();

      /* methods to indicate EOF and error symbol indexes */
      cout.WriteLine("  /** <code>EOF</code> Symbol index. */");
      cout.WriteLine("  public override int EOF_sym() {return " + terminal.EOF.index() + 
					  ";}");
      cout.WriteLine();
      cout.WriteLine("  /** <code>error</code> Symbol index. */");
      cout.WriteLine("  public override int error_sym() {return " + terminal.error.index() +
					  ";}");
      cout.WriteLine();

      /* user supplied code for user_init() */
      if (init_code != null)
	{
          cout.WriteLine();
	  cout.WriteLine("  /** User initialization code. */");
	  cout.WriteLine("  public override void user_init() ");
	  cout.WriteLine("    {");
	  cout.WriteLine(init_code);
	  cout.WriteLine("    }");
	}

      /* user supplied code for scan */
      if (scan_code != null)
	{
          cout.WriteLine();
	  cout.WriteLine("  /** Scan to get the next Symbol. */");
	  cout.WriteLine("  public override TUVienna.CS_CUP.Runtime.Symbol scan()");
	  cout.WriteLine("    {");
	  cout.WriteLine(scan_code);
	  cout.WriteLine("    }");
	}

      /* user supplied code */
      if (parser_code != null)
	{
	  cout.WriteLine();
          cout.WriteLine(parser_code);
	}

      /* end of class */
      cout.WriteLine("}");

      /* put out the action code class */
      emit_action_code(cout, start_prod);
      if (package_name!=null)
            cout.WriteLine("}");
      parser_time = DateTime.Now.Ticks - start_time;
    }

    /*-----------------------------------------------------------*/
}
}