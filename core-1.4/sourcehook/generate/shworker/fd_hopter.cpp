// From fastdelegate
// http://www.codeproject.com/cpp/FastDelegate.asp

// Modified to be run from shworker by PM


// HOPTER.EXE -- A simple header generator for vararg templates
/*  MOTIVATION
 C++ doesn't have a vararg feature for templates. This can be a
 nuisance when writing class libraries. There are a few options:
 (a) manually repeat code. This is tedious and error prone.
 (b) Use macros. Messy, can't cope with template
   arguments, and users get hard-to-interpret error messages.
 (c) Use the boost preprocessor library. This slows compilation,
   and introduces a dependency on boost.
 (d) Write a program to automatically generate the code.
   I've never seen a general program to do this job, so this is 
   a very simple command-line program to do it.

 Why is it called Hopter?
 I wanted a word that started with h, to be reminiscent of header
 files. At the time, my 2yo son kept talking about 'Hopter Copters'
 (he couldn't pronounce 'helicopter').
 It also reflects the level of sophistication of this program --
 it was named by a two year old.

 IMPLEMENTATION

 When you analyze the problem, you find that the requirements
 are very simple.
 Varargs are needed in two places: templates and functions.
 You want to use them in two ways: when declaring the template
 or function, and when invoking the function / selecting the 
 template.

 It's a brain-dead implementation, which just does a search-and-replace
 of specific strings. It could be done with a few lines using sed on
 a *NIX platform. Because I use Windows, I've done a quick and dirty
 implementation using CString.

In a file, @VARARGS at the start of a line indicates the start of the code
to be expanded. @ENDVAR at the start of a line marks the end of the 
expansion.

 declare template  --  template <@CLASSARGS> class SomeClass
 template<class Param1, class Param2>
 class SomeClass

 declare function  -- somefunc(@FUNCARGS)
 somefunc(Param1 p1, Param2 p2)

 select template -- SomeClass<@SELARGS>
 SomeClass<Param1, Param2>

 invoke function -- somefunc(@INVOKEARGS)
 somefunc(p1, p2)

 I also provide @NUM which is the number of arguments.
 The case where @NUM is zero is a special case: for template declarations,
 the entire template<> bit needs to disappear. When other arguments are 
 involved, either at the beginning or end of the list, there a commas 
 which might need to be removed.

 I've used .hxx as the extension for these files. This enables C++ 
 I wanted to use an unusual file extension (.HOH) for these files, because they
 aren't real header files. But I also like to use the C++ syntax highlighting 
 in Visual Studio. I chose .hxx because MSVC treats it as C++, but it 
 seems to very rarely used.

 Installation (for VC6, if you want to use .HOH as the extension instead):
 Go into the registry and change 
 HKEY_CURRENT_USER\Software\Microsoft\DevStudio\6.0\Text Editor\Tabs/Language Settings\C/C++\FileExtensions from cpp;cxx;c;h;hxx;hpp;inl;tlh;tli;rc;rc2 
 to cpp;cxx;c;h;hxx;hpp;inl;tlh;tli;rc;rc2;hoh by adding “;hoh” to the end.
 Then add this as a custom build step to the main file of your project:
 hopter $(InputDir)\*.hoh $(InputDir)\*.h

*/

#include <string>
#include <stdarg.h>
#include <stdio.h>

// This is a quick-n-dirty implementation of a CString replacement.
// It promises nothing more than to provide enough functionality to get
// this program to run.
// Note that I have really never used CString, so the information to
// implement the members that are used in this program come from
// online documentation at http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vcmfc98/html/_mfc_cstring.asp
// Also, I have only used VC++ a few times, a long time ago, so
// I really do not know what all their alphabet soup is suppoed to
// mean, so I just made a guess...


class CString
{
public:
  enum { MAX_FORMAT_STRING_SIZE = 8 * 1044 };
  CString()
    : str_()
  {
  }

  CString(
      CString const & rhs)
    : str_(rhs.str_)
  {
  }

  CString(
      char ch,
      int repeat = 1)
    : str_(repeat, ch)
  {
  }

  CString(
      char const * s)
    : str_(s)
  {
  }

  CString & operator=(
      CString const & rhs)
  {
    str_ = rhs.str_;
    return *this;
  }

  CString & operator=(
      char const * s)
  {
    str_ = s;
    return *this;
  }

  CString & operator=(
      char ch)
  {
    str_ = ch;
    return *this;
  }

  void Format(
      char const * fmt,
      ...)
  {
    char buffer[ MAX_FORMAT_STRING_SIZE ];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buffer, fmt, ap);
    va_end(ap);
    str_ = buffer;
  }

  char operator[](
      int x) const
  {
    return str_[x];
  }
  
  CString & operator+=(
      CString const & x)
  {
    str_ += x.str_; return *this;
  }

  int Replace(
      CString const & lhs,
      CString const & rhs)
  {
    int rval = 0;
    std::string::size_type pos = 0;
    while (pos < str_.length())
    {
      pos = str_.find(lhs.str_, pos);
      if (pos != std::string::npos)
      {
        str_.replace(pos, lhs.GetLength(), rhs.str_);
        pos += rhs.GetLength();
        ++rval;
      }
    }
    return rval;
  }

  int GetLength() const
  {
    return str_.length();
  }

  operator char const * () const
  {
    return str_.c_str();
  }

private:
  std::string str_;
};


CString operator+(
    CString const & x,
    CString const & y)
{
  CString result(x);
  result += y;
  return result;
}


/* Example: This is an excerpt from boost::function_template.hpp

#define BOOST_FUNCTION_FUNCTION_INVOKER \
  BOOST_JOIN(function_invoker,BOOST_FUNCTION_NUM_ARGS)
#define BOOST_FUNCTION_VOID_FUNCTION_INVOKER \
  BOOST_JOIN(void_function_invoker,BOOST_FUNCTION_NUM_ARGS)

      template<
        typename FunctionPtr,
        typename R BOOST_FUNCTION_COMMA
        BOOST_FUNCTION_TEMPLATE_PARMS
      >
      struct BOOST_FUNCTION_GET_FUNCTION_INVOKER
      {
        typedef typename ct_if<(is_void<R>::value),
                            BOOST_FUNCTION_VOID_FUNCTION_INVOKER<
                            FunctionPtr,
                            R BOOST_FUNCTION_COMMA
                            BOOST_FUNCTION_TEMPLATE_ARGS
                          >,
                          BOOST_FUNCTION_FUNCTION_INVOKER<
                            FunctionPtr,
                            R BOOST_FUNCTION_COMMA
                            BOOST_FUNCTION_TEMPLATE_ARGS
                          >
                       >::type type;
      };

Using HOPTER, this can be written as:

    template< typename FunctionPtr, typename R, @CLASSARGS>
      struct get_function_invoker@NUM
	  {
        typedef typename ct_if<(is_void<R>::value),
					void_function_invoker@NUM<FunctionPtr, R, @SELARGS>,
					function_invoker@NUM<FunctionPtr,R, @SELARGS>
                       >::type type;
      };
*/

int MaxNumArgs;

/// Makes the necessary subsititutions in 'bigblock', and writes to fout.
void PrintVarArgs(FILE *fout, CString bigblock, int num) {
	CString numstr;
	CString invokelist;
	CString funclist;
	CString selectlist;
	CString classlist;
	CString commastr;
	numstr.Format("%d", num);
	if (num==0) {
		invokelist="";
		funclist="";
		selectlist="";
		commastr="";
		classlist="";
	}else {
		invokelist="p1";
		funclist="Param1 p1";
		selectlist = "Param1";
		classlist = "class Param1";
		commastr=", ";
		CString str;
		for (int k=2; k<=num; k++) {
			str.Format(", p%d", k);
			invokelist+=str;
			str.Format(", Param%d p%d", k, k);
			funclist+=str;
			str.Format(", Param%d", k);
			selectlist += str;
			str.Format(", class Param%d", k);
			classlist += str;
		}
	}

	// Simple replacement of number of arguments
	bigblock.Replace("@NUM", numstr);

	// Template declarations
	if (num==0) bigblock.Replace("template<@CLASSARGS>", "");
	else bigblock.Replace("template<@CLASSARGS>", 
		(CString)"template<" + classlist+">");
	if (num==0) bigblock.Replace("template <@CLASSARGS>", "");
	else bigblock.Replace("template <@CLASSARGS>", 
		(CString)"template<" + classlist+">");
	bigblock.Replace(",@CLASSARGS", commastr + classlist);
	bigblock.Replace(", @CLASSARGS", commastr + classlist);
	bigblock.Replace("@CLASSARGS, ", classlist + commastr);
	bigblock.Replace("@CLASSARGS,", classlist + commastr);
	bigblock.Replace("@CLASSARGS", classlist);

	// Template selections
	CString selargstr;
	if (num==0) selargstr = "";
	else selargstr = (CString)"<"+selectlist+">";

	bigblock.Replace("<@SELARGS>", selargstr);
	bigblock.Replace("< @SELARGS >", selargstr);
	bigblock.Replace(",@SELARGS", commastr + selectlist);
	bigblock.Replace(", @SELARGS", commastr + selectlist);
	bigblock.Replace("@SELARGS, ", selectlist + commastr);
	bigblock.Replace("@SELARGS,", selectlist + commastr);
	bigblock.Replace("@SELARGS", selectlist);

	// Function declarations
	bigblock.Replace(",@FUNCARGS", commastr + funclist);
	bigblock.Replace(", @FUNCARGS", commastr + funclist);
	bigblock.Replace("@FUNCARGS, ", funclist + commastr);
	bigblock.Replace("@FUNCARGS,", funclist + commastr);
	bigblock.Replace("@FUNCARGS", funclist);

	// Function invocation
	bigblock.Replace(",@INVOKEARGS", commastr + invokelist);
	bigblock.Replace(", @INVOKEARGS", commastr + invokelist);
	bigblock.Replace("@INVOKEARGS, ", invokelist + commastr);
	bigblock.Replace("@INVOKEARGS,", invokelist + commastr);
	bigblock.Replace("@INVOKEARGS", invokelist);

	fprintf(fout, bigblock);				
}

int action_hopter(int numargs, const char *filenamein, const char *filenameout)
{

	MaxNumArgs = numargs;

	FILE * fin;
	FILE *fout;

	fin =fopen(filenamein,"rt");
	if (!fin) { printf("Error, can't open %s\n", filenamein); return 1; };
	fout = fopen (filenameout, "wt");
	if (!fout) { printf("Error, can't open %s\n", filenameout); return 1; };
	char buf[5000];
	for (;;) {
	 if (!fgets(buf, 5000, fin)) break;
		if (!strncmp(buf, "@VARARGS", 8)) {
			// found a match...
			CString bigblock;
			for (;;) {
				if (feof(fin)) { printf("No matching @ENDVAR !!??\n"); return 1; };
				fgets(buf, 5000, fin);
				if (!strncmp(buf, "@ENDVAR", 7)) break;
				bigblock+=buf;
			}
//			fprintf(fout, "// Auto-generated code [[[\n");
			for (int k=0; k<=MaxNumArgs; k++) {
				fprintf(fout, "//N=%d\n", k);
				PrintVarArgs(fout, bigblock, k);
			}
//			fprintf(fout, "// ]]] End auto-generated code\n");
		} else {
			fprintf(fout, "%s", buf);
		}
	}
	fclose(fin);
	fclose(fout);
	return 0;
}
