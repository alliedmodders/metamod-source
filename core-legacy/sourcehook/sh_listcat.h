/* ======== SourceMM ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

// Used for VP-Hooks

#ifndef _INCLUDE_SMM_LISTCAT_H
#define _INCLUDE_SMM_LISTCAT_H

#include "sh_list.h"

namespace SourceHook
{

	// Only a very special-case forward iterator!
	template <typename T>
	class ListCatIterator
	{
		List<T> *m_pListLeft;
		List<T> *m_pListRight;
		typename List<T>::iterator m_Iter;

		void CheckLeftEmptyOnBegin()
		{
			// If the left list is empty and right is valid, GoToBegin sets m_Iter to m_Left
			// End() checks for equality to m_Right.end() so it returns false
			// then Next() corrupts by incrementing!

			// To avoid this, we skip left if it's empty.
			if (m_pListLeft && m_pListLeft->empty() && m_pListRight)
			{
				m_Iter = m_pListRight->begin();
			}
		}
	public:
		// At least one list has to be non-null!
		ListCatIterator(List<T> *pListLeft, List<T> *pListRight) : m_pListLeft(pListLeft), m_pListRight(pListRight),
			m_Iter(pListLeft ? pListLeft->begin() : pListRight->begin())
		{
			CheckLeftEmptyOnBegin();
		}

		void GoToBegin()
		{
			m_Iter = m_pListLeft ? m_pListLeft->begin() : m_pListRight->begin();
			CheckLeftEmptyOnBegin();
		}

		bool End()
		{
			return m_pListRight ? (m_Iter == m_pListRight->end())
				: (m_Iter == m_pListLeft->end());
		}

		//pre increment
		ListCatIterator & operator++()
		{
			++m_Iter;
			if (m_pListLeft && m_Iter == m_pListLeft->end())
			{
				if (m_pListRight)
					m_Iter = m_pListRight->begin();
			}
			return *this;
		}
		//post increment
		ListCatIterator operator++(int)
		{
			ListCatIterator old(*this);
			
			++m_Iter;
			if (m_pListLeft && m_Iter == m_pListLeft->end())
			{
				if (m_pListRight)
					m_Iter = m_pListRight->begin();
			}

			return old;
		}

		const T & operator * () const
		{
			return *m_Iter;
		}
		T & operator * ()
		{
			return *m_Iter;
		}

		T * operator -> ()
		{
			return &(*m_Iter);
		}
		const T * operator -> () const
		{
			return &(*m_Iter);
		}

		bool operator != (const typename List<T>::iterator &where) const
		{
			return (m_Iter != where);
		}
		bool operator ==(const typename List<T>::iterator &where) const
		{
			return (m_Iter == where);
		}

		ListCatIterator & operator = (const typename List<T>::iterator &where)
		{
			m_Iter = where;

			if (m_pListLeft && m_Iter == m_pListLeft->end())
			{
				if (m_pListRight)
					m_Iter = m_pListRight->begin();

				// :HACK HACK: RemoveHookById is not aware of ListCatIterator (yet? :TODO: Change it!)
				// So we have to do this here... (look for the "Move all iterators pointing at this" section)
				--m_Iter;
			}

			return *this;
		}

		ListCatIterator & operator = (const ListCatIterator<T> &other)
		{
			m_Iter = other.m_Iter;
			m_pListLeft = other.m_pListLeft;
			m_pListRight = other.m_pListRight;
			return *this;
		}

		void SetListLeft(List<T> *pList)
		{
			m_pListLeft = pList;
		}
	};
}

#endif

