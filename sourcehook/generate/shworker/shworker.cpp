// SHWorker
// Inspired by "Hopter" that comes with FastDelegate (http://www.codeproject.com/cpp/FastDelegate.asp)
// Much more powerful (and ugly) though

/*

	INPUT FILE DIRECTIVES

	$a is the first additional argument, $b the second, ...

	---
	ITERATION

	@[variable,min,max:code|separator@]

	variable:	this will be replaced in code by its current value.
				vars are always $ and a number.
	min:		first value to be used for variable
	max:		last value to be used for variable
	code:		the code that will be inserted on each iteration.
	separator:	optional. this will be inserted between iterations.
				If you don't use a separator, you may leave out the |
	IMPORTANT:	iterations will only be performed if max >= min

	--- ARITHMETIC EXPRESSION

	@(expr)

	expr may contain:
		variables
		constants
		operators (currently only + and * are supported)

	--- CONDITION

	@[expr operator expr:code@]

	Example: @[$1!=0:hello@]

	Currently only != and == are supported operators.


	Yes, error handling in here is weird, some stuff uses return values, other code uses exceptions.
*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <stack>
#include <map>
#include "stdio.h"

#ifdef __linux__
# define stricmp strcasecmp
#endif

// Ensure that the template version is being used!
#ifdef min
#undef min
#endif

using namespace std;

extern int action_hopter(int numargs, const char *filenamein, const char *filenameout);

typedef map<int,int> varmap;

struct MyError
{
	const char *m_desc;

	MyError(const char *desc) : m_desc(desc)
	{
	}

	void Print()
	{
		cout << m_desc << endl;
	}
};

struct SyntaxError : MyError
{
	SyntaxError() : MyError("Syntax error in expression")
	{
	}
};
struct OtherError : MyError
{
	OtherError() : MyError("WTF")
	{
	}
};

void trim_string(std::string &str)
{
	size_t first = str.find_first_not_of(" \t\v\n\r");
	if (first == std::string::npos)
	{
		str.clear();
		return;
	}

	size_t last = str.length();
	for (std::string::reverse_iterator riter = str.rbegin(); riter != str.rend(); ++riter)
	{
		char ch = *riter;
		if (ch != ' '	&&
			ch != '\t'	&&
			ch != '\v'	&&
			ch != '\n'	&&
			ch != '\r')
			break;
		--last;
	}
	str = str.substr(first, last - first);
}

// unused
bool ExtractToken(std::string &strin, std::string &strout)
{
	trim_string(strin);
	if (strin.begin() == strin.end())
	{
		strout.clear();
		return false;
	}
	size_t first = strin.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFHIJKLMNOPQRSTUVWXYZ_0123456789");
	if (first == 0)
	{
		if (strin.size() > 1 && strin.at(0) == '/' && strin.at(1) == '/')
		{
			// One-line comment, find its end
			first = strin.find('\n') + 1;
		}
		else if (strin.size() > 1 && strin.at(0) == '/' && strin.at(1) == '*')
		{
			// Multi-line comment, find its end
			first = strin.find("*/") + 2;
		}
		strin = strin.substr(1);
		strout.clear();
		return true;
	}
	strout = strin.substr(0, first);
	strin = strin.substr(first);
	return true;
}

// Returns the number of occurencies replaced
int DoReplace(string &str, const string &what, const string &with)
{
	int cnt=0;
	size_t where = str.find(what);

	while (where != string::npos)
	{
		str.replace(where, what.size(), with);
		++cnt;
		where = str.find(what, where);
	}
	return cnt;
}

int DoReplace(string &str, const char *what, const char *with)
{
	int cnt=0;
	size_t where = str.find(what);
	size_t whatsize = strlen(what);
	while (where != string::npos)
	{
		str.replace(where, whatsize, with);
		++cnt;
		where = str.find(what, where);
	}
	return cnt;
}


class ExprParser
{
	// grammar:
	/*
	expr -> expr + term			{ do_add }
	| expr - term			{ do_sub }
	| term

	term -> term * factor		{ do_mul }
	| term / factor		{ do_div }
	| term % factor		{ do_mod }

	factor -> (expr)
	| numeric constant	{ push }


	equivalent to:

	expr -> term moreterms
	moreterms -> + term { do_add } moreterms
	moreterms -> - term { do_sub } moreterms
	moreterms -> epsilon

	term -> factor morefactors
	morefactors -> * factor { do_mul } morefactors
	morefactors -> / factor { do_div } morefactors
	morefactors -> % factor { do_mod } morefactors
	morefactors -> epsilon

	factor -> (expr)
	factor -> numeric constant { push }

	*/

	string::const_iterator m_begin;
	string::const_iterator m_end;
	string::const_iterator m_iter;

	int m_lookahead;
	int m_tokenval;

	stack<int> m_stack;
	static const int DONE = 256;
	static const int NUM = 257;

	int lexan()
	{
		while (1)
		{
			if (m_iter == m_end)
				return DONE;

			int t = *m_iter++;

			if (t == ' ' || t == '\t')
				;			// Remove whitespace
			else if (isdigit(t))
			{
				--m_iter;

				m_tokenval = 0;
				while (m_iter != m_end && isdigit(*m_iter))
				{
					m_tokenval *= 10;
					m_tokenval += *m_iter - '0';
					++m_iter;
				}
				return NUM;
			}
			else
				return t;
		}
	}

	void match(int t)
	{
		if (m_lookahead == t)
			m_lookahead = lexan();
		else
			throw SyntaxError();
	}
	void factor()
	{
		switch (m_lookahead)
		{
		case '(':
			match('('); expr(); match(')');
			break;
		case NUM:
			m_stack.push(m_tokenval); match(NUM);
			break;
		default:
			throw SyntaxError();
		}
	}
	void term()
	{
		factor();
		while (1)
		{
			switch (m_lookahead)
			{
			case '*':
				match('*'); factor(); do_mul();
				continue;
			case '/':
				match('/'); factor(); do_div();
				continue;
			case '%':
				match('%'); factor(); do_mod();
				continue;
			default:
				return;
			}

		}
	}

	void expr()
	{
		term();
		while (1)
		{
			switch (m_lookahead)
			{
			case '+':
				match('+'); term(); do_add();
				continue;
			case '-':
				match('-'); term(); do_sub();
				continue;
			default:
				return;
			}
		}
	}

	void do_add()
	{
		int a2 = m_stack.top(); m_stack.pop();
		int a1 = m_stack.top(); m_stack.pop();
		m_stack.push(a1 + a2);
	}

	void do_sub()
	{
		int a2 = m_stack.top(); m_stack.pop();
		int a1 = m_stack.top(); m_stack.pop();
		m_stack.push(a1 - a2);
	}

	void do_mul()
	{
		int a2 = m_stack.top(); m_stack.pop();
		int a1 = m_stack.top(); m_stack.pop();
		m_stack.push(a1 * a2);
	}

	void do_div()
	{
		int a2 = m_stack.top(); m_stack.pop();
		int a1 = m_stack.top(); m_stack.pop();
		m_stack.push(a1 / a2);
	}

	void do_mod()
	{
		int a2 = m_stack.top(); m_stack.pop();
		int a1 = m_stack.top(); m_stack.pop();
		m_stack.push(a1 % a2);
	}

public:
	ExprParser(string::const_iterator begin, string::const_iterator end) :
	m_begin(begin), m_end(end), m_iter(begin)
	{
		m_lookahead = lexan();
		expr();
	}

	operator int()
	{
		if (m_stack.size() != 1)
			throw OtherError();

		return m_stack.top();
	}
};

int parse_expr(string::const_iterator begin, string::const_iterator end)
{
	return ExprParser(begin, end);
}

size_t find_first_directive(const string &buf, size_t begin=0)
{
	for (;;)
	{
		if (begin >= buf.size())
			return string::npos;

		size_t firstdirpos = buf.find('@', begin);
		if (firstdirpos == string::npos)
			return firstdirpos;

		if (buf.size() > firstdirpos+1)
		{
			if (buf[firstdirpos+1] == '[' || buf[firstdirpos+1] == '(')
				return firstdirpos;
		}
		begin = firstdirpos+1;
	}
}

// buf begins with a section. Find its end!
size_t find_section_end(const string &buf)
{
	int starttype = buf[1];
	int endtype = (buf[1] == '(') ? ')' : ']';

	int nestlevel = 0;

	if (starttype == '(')
	{
		for (string::const_iterator iter = buf.begin(); iter != buf.end(); ++iter)
		{
			if (*iter == starttype)
				++nestlevel;
			if (*iter == endtype)
			{
				if (--nestlevel == 0)
					return iter - buf.begin() + 1;
			}
		}
		return string::npos;
	}
	else if (starttype == '[')
	{
		int lastchar = 0;
		for (string::const_iterator iter = buf.begin(); iter != buf.end(); ++iter)
		{
			if (lastchar == '@' && *iter == starttype)
				++nestlevel;
			if (lastchar == '@' && *iter == endtype)
			{
				if (--nestlevel == 0)
					return iter - buf.begin() + 1;
			}
			lastchar = *iter;
		}
		return string::npos;
	}
	
	return string::npos;
}

// replaces variables and additional arguments
void replace_vars(string &buf, int argc, int *argv, const varmap &vars)
{
	char varname[] = "$ ";
	char value[32];
	
	for (int i = 0; i < argc; ++i)
	{
		varname[1] = 'a' + i;
		sprintf(value, "%d", argv[i]);
		DoReplace(buf, varname, value);
	}

	for (varmap::const_iterator iter = vars.begin(); iter != vars.end(); ++iter)
	{
		varname[1] = '0' + iter->first;
		sprintf(value, "%d", iter->second);
		DoReplace(buf, varname, value);
	}
}

// do_input
// params:
//  argc:			number of additional arguments
//	argv:			additional arguments
//  outfile:		output file
//  buf:			string to be processed.  IMPORTANT: buf is modified!
//  curvars:		variables buffer.
// retval:
//  0 on success, non-zero on error

int do_input(int argc, int *argv, ofstream &outfile, string &buf, varmap &curvars)
{
	for (;;)
	{
		// Find the next directive.
		size_t firstdirpos = find_first_directive(buf);

		// Output everything that came before, and remove it from buf
		outfile << buf.substr(0, firstdirpos);
		if (firstdirpos == string::npos)
			return 0;
		buf = buf.substr(firstdirpos);

		// Now find the matching end.
		size_t sectionend = find_section_end(buf);
		if (sectionend == string::npos)
		{
			cout << "ERROR: Section not closed!" << endl;
			return 1;
		}

		// Place the section in its own buffer and remove it from the input string.
		string sect(buf.begin(), buf.begin() + sectionend);
		buf = buf.substr(sectionend);

		// CASE1: Arithmetic expression
		if (sect[1] == '(')
		{
			replace_vars(sect, argc, argv, curvars);
			outfile << parse_expr(sect.begin()+1, sect.end());
		}
		else if (sect[1] == '[')
		{
			int is_iter = 0;	// 0 -> no; 1 -> maybe (only used in check); 2 -> yes
			char lastchar = 0;
			// This could be an iteration OR a conditional thing.
			// Pretty braindead check: iterations begin with a variable, then a comma.
			for (string::iterator iter = sect.begin() + 2; iter != sect.end(); ++iter)
			{
				if (*iter == ' ' || *iter == '\t')
					;
				else if (is_iter == 0 && lastchar == '$' && isdigit(*iter))
					is_iter = 1;
				else if (is_iter == 1 && *iter == ',')
				{
					is_iter = 2;
					break;
				}
				else if (*iter == '$')
					;
				else
					break;
				lastchar = *iter;
			}
			if (is_iter == 2)
			{
				// CASE2: iteration
				// Looks like: @[var,min,max:code|sep@]
				// Replace known variables / additional arguments
				replace_vars(sect, argc, argv, curvars);

				// get the parts!
				string varname;
				int varnum;
				int expr_min;
				int expr_max;

				// varname
				size_t comma = sect.find(',');
				if (comma == string::npos)
				{
					cout << "Invalid iteration syntax" << endl;
					return 1;
				}
				varname.assign(sect.begin() + 2, sect.begin() + comma);
				trim_string(varname);
				if (varname.size() != 2 || varname[0] != '$' || !isdigit(varname[1]))
				{
					cout << "Invalid variable name" << endl;
					return 1;
				}
				varnum = varname[1] - '0';

				// min
				++comma;
				size_t nextcomma = sect.find(',', comma);
				if (nextcomma == string::npos)
				{
					cout << "Invalid iteration syntax" << endl;
					return 1;
				}
				expr_min = parse_expr(sect.begin() + comma, sect.begin() + nextcomma);

				// max
				comma = nextcomma + 1;
				nextcomma = sect.find(':', comma);
				if (nextcomma == string::npos)
				{
					cout << "Invalid iteration syntax" << endl;
					return 1;
				}

				expr_max = parse_expr(sect.begin() + comma, sect.begin() + nextcomma);
				
				// separator
				size_t sepbegin = sect.find('|');
				size_t sepend = string::npos;
				if (sepbegin != string::npos && sepbegin < nextcomma)
				{
					// There's a separator!
					++sepbegin;
					sepend = nextcomma;
				}
				else
					sepbegin = string::npos;


				++nextcomma; // nextcomma is now where code begins!

				size_t codeend = sect.size() - 2;

				// Check whether the var is already taken
				if (curvars.find(varnum) != curvars.end())
				{
					cout << "Variable $" << varnum << "already taken!" << endl;
					return 1;
				}

				// Do iterations!!
				for (int i = expr_min; i <= expr_max; ++i)
				{
					curvars[varnum] = i;

					string code(sect.begin() + nextcomma, sect.begin() + codeend);
					replace_vars(code, argc, argv, curvars);

					// Feed it through the input routine (RECURSE!!!!!! YEAH!)
					do_input(argc, argv, outfile, code, curvars);
					
					// Add separator if required
					if (sepbegin != string::npos && i != expr_max)
					{
						string tmp(sect.begin() + sepbegin, sect.begin() + sepend);
						do_input(argc, argv, outfile, tmp, curvars);
					}
				}
				// Remove the var!
				curvars.erase(varnum);
			}
			else
			{
				// CASE3: conditional thing.
				// Looks like: @[expr1 operator expr2:code@]
				
				// Find the operator position

				enum OP_TYPE
				{
					OP_EQ,
					OP_NEQ
				};

				OP_TYPE op;
				size_t oppos = sect.find("==");
				if (oppos != string::npos)
					op = OP_EQ;
				else
				{
					oppos = sect.find("!=");
					if (oppos != string::npos)
						op = OP_NEQ;
					else
					{
						cout << "Conditional expression without operator!?" << endl;
						return 1;
					}
				}
				size_t colon = sect.find(':');

				// Now we've got everything. Parse first expr:
				int expr1 = parse_expr(sect.begin() + 2, sect.begin() + oppos);
				int expr2 = parse_expr(sect.begin() + oppos + 2, sect.begin() + colon);
				if ((op == OP_EQ  && expr1 == expr2) ||
					(op == OP_NEQ && expr1 != expr2))
				{
					// Condition is true, process it!
					// The text may still contain arithmetic exprs or other cond. exprs
					// so send it through do_input
					string tmp(sect.substr(colon+1, sect.size() - colon - 3));
					do_input(argc, argv, outfile, tmp, curvars);
				}
			}
		}
		else
		{
			cout << "WTF" << endl;
			return 1;
		}
	}
	
	return 0;
}



// action_iter
// params:
//  filenamein:		input file name
//  filenameout:	output file name
//  argc:			number of additional arguments
//	argv:			additional arguments
// retval: 0 on success, non-zero on error

// Convert additional arguments 
// Read whole input file to memory and open output file
// Pass to do_input()
int action_iter(const char *filenamein, const char *filenameout, int argc, const char *argv[])
{
	// Convert additional arguments
	const int MAX_ARGC = 10;
	int converted_argv[MAX_ARGC];

	int i;
	for (i = 0; i < argc && i < MAX_ARGC; ++i)
		converted_argv[i] = atoi(argv[i]);

	if (argc != i)
		cout << "WARNING: Not all additional arguments processed!" << endl;


	// Read whole input file to memory and open output file
	ifstream fin(filenamein);
	ofstream fout(filenameout);

	if (!fin)
	{
		cout << "Could not open file \"" << filenamein << "\"." << endl;
		return 1;
	}
	if (!fout)
	{
		cout << "Could not open file \"" << filenameout << "\"." << endl;
		return 1;
	}
	string input_str(
		istreambuf_iterator<char> (fin.rdbuf()),
		istreambuf_iterator<char> ());


	// Begin processing input
	varmap vars;
	try
	{
		return do_input(argc, converted_argv, fout, input_str, vars);
	}
	catch (MyError err)
	{
		err.Print();
		return 1;
	}
}

// MAIN
//  Prints usage if required
//  Calls action_hopter OR action_iter
int main(int argc, const char **argv)
{
	if (argc < 4)
	{
		cout << "Usage:" << endl << "   shworker [iter/hopter] ..." << endl;
		cout << "   shworker iter filename.in filename.out [param1, param2, ...]" << endl;
		cout << "   shworker hopter filename.in filename.out [num-of-args]" << endl;
		return 1;
	}

	const char *action = argv[1];

	if (stricmp(action, "hopter") == 0)
	{
		const char *filenamein = argv[2];
		const char *filenameout = argv[3];
		int argsnum = atoi(argv[4]);

		return action_hopter(argsnum, filenamein, filenameout); 
	}
	else if (stricmp(action, "iter") == 0)
	{
		const char *filenamein = argv[2];
		const char *filenameout = argv[3];
		int additional_argc = argc - 4;
		const char ** additional_argv = argv + 4;
		return action_iter(filenamein, filenameout, additional_argc, additional_argv);
	}
	else
	{
		cout << "Unrecognized action: " << argv[1] << endl;
		return 1;
	}
}
