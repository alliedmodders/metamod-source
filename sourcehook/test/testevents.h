/* ======== SourceHook ========
* Copyright (C) 2004-2008 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __TESTEVENTS_H__
#define __TESTEVENTS_H__

#include <typeinfo>
#include <stdarg.h>
#include <iostream>
#include <list>
#include <string>

extern bool g_Verbose;

static unsigned int MakeHash(const char *name)
{
	int a = 0;
	unsigned int res = 0xFFFFFFFF;

	while (*name)
	{
		res ^= ((unsigned int)*name << ((a++ % 4)*8));
		++name;
	}
	return res;
}

struct State
{
	virtual ~State()
	{

	}

	virtual bool IsEqual(State *other)
	{
		return (MakeHash(GetName()) == MakeHash(other->GetName())) ? true : false;
	}

	virtual bool Ignore()
	{
		return false;
	}

	virtual void Dump() = 0;
	virtual const char *GetName() = 0;
};

struct IgnoreState : public State
{
	virtual bool Ignore()
	{
		return true;
	}
	virtual void Dump()
	{
	}
};

typedef std::list<State*> StateList;
namespace
{
	void DumpStates(StateList *sl)
	{
		for (StateList::iterator iter = sl->begin(); iter != sl->end(); ++iter)
			(*iter)->Dump();
	}

	bool StatesOk(StateList *sl, ...)
	{
		StateList requiredstates;
		va_list argptr;
		va_start(argptr, sl);
		while (true)
		{
			State *cs = va_arg(argptr, State*);
			if (!cs)
				break;
			if (cs->Ignore())
				continue;
			requiredstates.push_back(cs);
		}
		va_end(argptr);

		if (requiredstates.size() != sl->size())
		{
			if (g_Verbose)
			{
				std::cout << std::endl << "FAIL: Should be:" << std::endl;
				DumpStates(&requiredstates);
				std::cout << std::endl << "FAIL: Is:" << std::endl;
				DumpStates(sl);
			}

			for (StateList::iterator iter = requiredstates.begin(); iter != requiredstates.end(); ++iter)
				delete *iter;
			for (StateList::iterator iter = sl->begin(); iter != sl->end(); ++iter)
				delete *iter;
			sl->clear();
			return false;
		}

		bool ok = true;
		StateList::iterator req_iter = requiredstates.begin();
		for (StateList::iterator o_iter = sl->begin(); o_iter != sl->end(); ++o_iter, ++req_iter)
		{
			if (!(*o_iter)->IsEqual(*req_iter))
			{
				ok = false;
				break;
			}
		}

		if (!ok && g_Verbose)
		{
			std::cout << std::endl << "FAIL: Should be:" << std::endl;
			DumpStates(&requiredstates);
			std::cout << std::endl << "FAIL: Is:" << std::endl;
			DumpStates(sl);
		}

		for (StateList::iterator iter = requiredstates.begin(); iter != requiredstates.end(); ++iter)
			delete *iter;
		for (StateList::iterator iter = sl->begin(); iter != sl->end(); ++iter)
			delete *iter;
		sl->clear();

		return ok;
	}
}

#define ADD_STATE(name) g_States.push_back(new name)
#define ADD_STATE_PTR(statesptr, name) statesptr->push_back(new name)

#define CHECK_STATES(mwah, myerr) if (!StatesOk mwah) { error=myerr; return false; } else if (g_Verbose) { std::cout << "No error: " << myerr << std::endl; }

#define MAKE_STATE(name) struct name : State { \
		virtual void Dump() { \
			std::cout << "  " << #name << std::endl; } \
		const char *GetName() { return #name; } \
	};

#define MAKE_STATE_1(name, p1_type) struct name : State { \
		p1_type m_Param1; \
		name(p1_type param1) : m_Param1(param1) {} \
		virtual bool IsEqual(State *other) { \
			if (MakeHash(GetName()) != MakeHash(other->GetName())) \
				return false; \
			name *other2 = static_cast<name*>(other); \
			return other2->m_Param1 == m_Param1;\
		} \
		virtual void Dump() { \
			std::cout << "  " << #name << "; Param1=" << m_Param1 << std::endl; } \
		const char *GetName() { return #name; } \
	}

#define MAKE_STATE_2(name, p1_type, p2_type) struct name : State { \
		p1_type m_Param1; \
		p2_type m_Param2; \
		name(p1_type param1, p2_type param2) : m_Param1(param1), m_Param2(param2) {} \
		virtual bool IsEqual(State *other) { \
			if (MakeHash(GetName()) != MakeHash(other->GetName())) \
				return false; \
			name *other2 = static_cast<name*>(other); \
			return other2->m_Param1 == m_Param1 && other2->m_Param2 == m_Param2;\
		} \
		virtual void Dump() { \
			std::cout << "  " << #name << "; Param1=" << m_Param1 << "; Param2=" << m_Param2 << std::endl; } \
		const char *GetName() { return #name; } \
	}

#define MAKE_STATE_3(name, p1_type, p2_type, p3_type) struct name : State { \
		p1_type m_Param1; \
		p2_type m_Param2; \
		p3_type m_Param3; \
		name(p1_type param1, p2_type param2, p3_type param3) : m_Param1(param1), m_Param2(param2), m_Param3(param3) {} \
		virtual bool IsEqual(State *other) { \
			if (MakeHash(GetName()) != MakeHash(other->GetName())) \
				return false; \
			name *other2 = static_cast<name*>(other); \
			return other2->m_Param1 == m_Param1 && other2->m_Param2 == m_Param2 && other2->m_Param3 == m_Param3;\
		} \
		virtual void Dump() { \
			std::cout << "  " << #name << "; Param1=" << m_Param1 << "; Param2=" << m_Param2 << "; Param3=" << m_Param3 << std::endl; } \
		const char *GetName() { return #name; } \
	}

#define MAKE_STATE_4(name, p1_type, p2_type, p3_type, p4_type) struct name : State { \
		p1_type m_Param1; \
		p2_type m_Param2; \
		p3_type m_Param3; \
		p4_type m_Param4; \
		name(p1_type param1, p2_type param2, p3_type param3, p4_type param4) : m_Param1(param1), m_Param2(param2), m_Param3(param3), m_Param4(param4) {} \
		virtual bool IsEqual(State *other) { \
			if (MakeHash(GetName()) != MakeHash(other->GetName())) \
				return false; \
			name *other2 = static_cast<name*>(other); \
			return other2->m_Param1 == m_Param1 && other2->m_Param2 == m_Param2 && other2->m_Param3 == m_Param3 && other2->m_Param4 == m_Param4;\
		} \
		virtual void Dump() { \
			std::cout << "  " << #name << "; Param1=" << m_Param1 << "; Param2=" << m_Param2 << "; Param3=" << m_Param3 << "; Param4=" << m_Param4 << std::endl; } \
		const char *GetName() { return #name; } \
	}

#define CHECK_COND(c, err) if (!(c)) { error = err; return false; }

#endif
