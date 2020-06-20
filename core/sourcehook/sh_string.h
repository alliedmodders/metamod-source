/* ======== SourceMM ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

/* AMX Mod X
 *
 * by the AMX Mod X Development Team
 */

#ifndef _INCLUDE_CSTRING_H
#define _INCLUDE_CSTRING_H

#include <string.h>
#include <stdio.h>

namespace SourceHook
{
class String
{
public:
	String() 
	{
		v = NULL;
		a_size = 0;
		//assign("");
	}

	~String()
	{ 
		if (v) 
			delete [] v; 
	}

	String(const char *src) 
	{
		v = NULL; 
		a_size = 0;
		assign(src); 
	}

	String(const String &src) 
	{
		v = NULL;
		a_size = 0;
		assign(src.c_str()); 
	}

	bool operator ==(const String &other)
	{
		return (compare(other.c_str()) == 0);
	}

	bool operator ==(const char *other)
	{
		return (compare(other) == 0);
	}

	const char *c_str() { return v?v:""; }

	const char *c_str() const { return v?v:""; }

	void append(const char *t)
	{
		Grow(size() + strlen(t) + 1);
		strcat(v, t);
	}

	void append(const char c)
	{
		size_t len = size();
		Grow(len + 2);
		v[len] = c;
		v[len + 1] = '\0';
	}

	void append(String &d)
	{
		append(d.c_str());
	}

	void assign(const String &src)
	{
		assign(src.c_str());
	}

	void assign(const char *d)
	{
		if (!d)
		{
			clear();
		} else {
			Grow(strlen(d) + 1, false);
			strcpy(v, d);
		}
	}

	void clear()
	{
		if (v)
			v[0] = '\0';
	}

	int compare (const char *d) const
	{
		if (!v)
			return strcmp("", d);
		else
			return strcmp(v, d);
	}

	//Added this for amxx inclusion
	bool empty() const
	{
		if (!v)
			return true;

		if (v[0] == '\0')
			return true;

		return false;
	}

	size_t size() const
	{
		if (v)
			return strlen(v);
		else
			return 0;
	}

	size_t find(const char c, size_t index = 0) const
	{
		size_t len = size();
		if (len < 1)
			return npos;
		if (index >= len)
			return npos;
		size_t i = 0;
		for (i=index; i<len; i++)
		{
			if (v[i] == c)
			{
				return i;
			}
		}

		return npos;
	}

	size_t find_last_of(const char c, size_t index = npos) const
	{
		size_t len = size();
		if (len < 1)
			return npos;
		if (index >= len)
			return npos;
		size_t i;
		if (index == npos)
			i = len - 1;
		else
			i = index;

		for (; i+1>0; i--)
		{
			if (v[i] == c)
			{
				return i;
			}
		}

		return npos;
	}

	bool is_space(int c) const
	{
		if (c == '\f' || c == '\n' ||
			c == '\t' || c == '\r' ||
			c == '\v' || c == ' ')
		{
			return true;
		}

		return false;
	}
	
	void trim()
	{
		if (!v)
			return;

		size_t i = 0;
		size_t j = 0;
		size_t len = strlen(v);

		if (len == 1)
		{
			if (is_space(v[i]))
			{
				clear();
				return;
			} 
		}

		unsigned char c0 = v[0];

		if (is_space(c0))
		{
			for (i=0; i<len; i++)
			{
				if (!is_space(v[i]) || (is_space(v[i]) && (i==len-1)))
				{
					erase(0, i);
					break;
				}
			}
		}

		len = strlen(v);

		if (len < 1)
		{
			return;
		}

		if (is_space(v[len-1]))
		{
			for (i=len-1; i<len; i--)
			{
				if (!is_space(v[i])
					|| (is_space(v[i]) && i==0))
				{
					erase(i+1, j);
					break;
				}
				j++;
			}
		}

		if (len == 1)
		{
			if (is_space(v[0]))
			{
				clear();
				return;
			}
		}
	}

	void erase(size_t start, size_t num = npos)
	{
		if (!v)
			return;
		size_t i = 0;
		size_t len = size();
		//check for bounds
		if (num == npos || start+num > len-start)
			num = len - start;
		//do the erasing
		bool copyflag = false;
		for (i=0; i<len; i++)
		{
			if (i>=start && i<start+num)
			{
				if (i+num < len)
				{	
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
				copyflag = true;
			} else if (copyflag) {
				if (i+num < len)
				{
					v[i] = v[i+num];
				} else {
					v[i] = 0;
				}
			}
		}
		len -= num;
		v[len] = 0;
	}

	String substr(size_t index, size_t num = npos) const
	{
		if (!v)
		{
			String b("");
			return b;
		}

		String ns;

		size_t len = size();

		if (index >= len || !v)
			return ns;
		
		if (num == npos)
		{
			num = len - index;
		} else if (index+num >= len) {
			num = len - index;
		}

		size_t i = 0;
		size_t nslen = num + 2;

		ns.Grow(nslen);

		for (i=index; i<index+num; i++)
			ns.append(v[i]);

		return ns;
	}

	void toLower()
	{
		if (!v)
			return;
		size_t i = 0;
		size_t len = strlen(v);
		for (i=0; i<len; i++)
		{
			if (v[i] >= 65 && v[i] <= 90)
				v[i] &= ~(1<<5);
		}
	}

	String & operator = (const String &src)
	{
		assign(src);
		return *this;
	}

	String & operator = (const char *src)
	{
		assign(src);
		return *this;

	}

	char operator [] (size_t index) const
	{
		if (index >= size() || !v)
		{
			return -1;
		} else {
			return v[index];
		}
	}

	int at(size_t a) const
	{
		if (a >= size() || !v)
			return -1;

		return v[a];
	}

	bool at(size_t at, char c)
	{
		if (at >= size() || !v)
			return false;

		v[at] = c;

		return true;
	}

private:
	void Grow(size_t d, bool copy=true)
	{
		if (d <= a_size)
			return;
		char *n = new char[d + 1];
		if (copy && v)
			strcpy(n, v);
		if (v)
			delete [] v;
		else
			strcpy(n, "");
		v = n;
		a_size = d + 1;
	}

	char *v;
	size_t a_size;
public:
	static const size_t npos = static_cast<size_t>(-1);
};

};	//NAMESPACE

#endif //_INCLUDE_CSTRING_H
