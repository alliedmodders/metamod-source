// SHWorker
// Inspired by "Hopter" that comes with FastDelegate (http://www.codeproject.com/cpp/FastDelegate.asp)
// Much more powerful (and ugly) though

// Action: iter
// @VARARGS@						begins session
// @..@								is replaced by .. if num!=0 and removed if num=0. Any
//									occurences of %% are replaced by the current iter
// @$@								is replaced by the current iter
// @ENDARGS@						ends session

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <list>
#include "stdio.h"

#ifdef __linux__
# define stricmp strcasecmp
#endif

using namespace std;

extern int hopter(int numargs, const char *filenamein, const char *filenameout);


void TrimString(std::string &str)
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

bool ExtractToken(std::string &strin, std::string &strout)
{
	TrimString(strin);
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

int main(int argc, char *argv[])
{
	if (argc < 5)
	{
		cout << "Usage:" << endl << "   shworker [iter/hopter] ..." << endl;
		cout << "   shworker iter [num-of-args] filename.in filename.out" << endl;
		cout << "   shworker hopter [num-of-args] filename.in filename.our" << endl;
		return 1;
	}

	const char *action = argv[1];
	int argsnum = atoi(argv[2]);
	const char *filenamein = argv[3];
	const char *filenameout = argv[4];

	if (stricmp(action, "hopter") == 0)
	{
		return hopter(argsnum, filenamein, filenameout);
	}

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

	if (stricmp(action, "iter") == 0)
	{
		// Generate versions of func macros for many parameters
		string str;
		string replacebuf;
		while (!fin.eof())
		{
			getline(fin, str);
			if (str == "@VARARGS@")
			{
				replacebuf.clear();

				while (true)
				{
					getline(fin, str);
					if (str == "@ENDARGS@")
						break;

					replacebuf += str;
					replacebuf += "\n";
					if (fin.eof())
					{
						cout << "No matching @ENDARGS@ !" << endl;
						return 1;
					}
				}

				for (int i = 0; i <= argsnum; ++i)
				{
					string cur = replacebuf;
					size_t where = cur.find('@');

					while (where != string::npos)
					{
						size_t where2 = cur.find('@', where+1);
						if (where2 == string::npos)
						{
							cout << "No ending @";
							return 1;
						}
						string tmp = cur.substr(where + 1, where2 - where - 1);
						cur.erase(where, where2 - where + 1);

						if (tmp.size() != 0 && tmp[0] == '$')
						{
							int tmp_out = i;
							if (tmp.size() > 2)
							{
								if (tmp[1] == '+')
								{
									istringstream istr(tmp.substr(2));
									int tmp;
									istr >> tmp;
									tmp_out += tmp;
								}
								else if (tmp[1] == '*')
								{
									istringstream istr(tmp.substr(2));
									int tmp;
									istr >> tmp;
									tmp_out *= tmp;
								}
							}
							stringstream istr;
							istr << tmp_out;
							cur.insert(where, istr.str());
							where = cur.find('@', where);
							continue;
						}

						// Find the |.. part, if any
						string tmpsep;
						size_t tmpsepbegin = tmp.rfind('|');
						if (tmpsepbegin != string::npos)
						{
							tmpsep = tmp.substr(tmpsepbegin + 1);
							tmp.erase(tmpsepbegin);
						}

						size_t pos = where;
						if (tmp.find("%%") == string::npos)
						{
							if (i > 0)
							{
								cur.insert(pos, tmp);
								pos += tmp.size();
							}
						}
						else
						{
							for (int j = 1; j <= i; ++j)
							{
								stringstream istr;
								istr << j;
								string what = tmp;
								DoReplace(what, string("%%"), istr.str());
								cur.insert(pos, what);
								pos += what.size();
								if (j != i)
								{
									cur.insert(pos, tmpsep);
									pos += tmpsep.size();
								}
							}
						}

						where = cur.find('@', where);
					}
					fout << cur;
				}
			}
			else
			{
				fout << str << endl;
			}
		}
	}
}
