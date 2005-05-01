#ifndef __TESTEVENTS_H__
#define __TESTEVENTS_H__

#include <typeinfo>
#include <stdarg.h>

namespace
{

	struct State
	{
		virtual ~State()
		{

		}

		virtual bool IsEqual(State *other)
		{
			return (typeid(other) == typeid(this)) ? true : false;
		}
	};

	typedef std::list<State*> StateList;

	#define ADD_STATE(name) g_States.push_back(new name)


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
			requiredstates.push_back(cs);
		}
		va_end(argptr);

		if (requiredstates.size() != sl->size())
		{
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

		for (StateList::iterator iter = requiredstates.begin(); iter != requiredstates.end(); ++iter)
			delete *iter;
		for (StateList::iterator iter = sl->begin(); iter != sl->end(); ++iter)
			delete *iter;
		sl->clear();

		return ok;
	}
}

#define CHECK_STATES(mwah, myerr) if (!StatesOk mwah) { error=myerr; return false; }

#endif

