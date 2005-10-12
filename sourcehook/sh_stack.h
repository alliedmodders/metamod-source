/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SH_STACK_H__
#define __SH_STACK_H__

#define SH_STACK_DEFAULT_SIZE 4

#include <assert.h>

namespace SourceHook
{
	// Vector
	template <class T> class CStack
	{
		T *m_Elements;
		size_t m_AllocatedSize;
		size_t m_UsedSize;
	public:
		friend class iterator;
		class iterator
		{
			CStack<T> *m_pParent;
			size_t m_Index;
		public:
			iterator(CStack<T> *pParent, size_t id) : m_pParent(pParent), m_Index(id)
			{
			}

			iterator(CStack<T> *pParent) : m_pParent(pParent), m_Index(0)
			{
			}
			
			iterator() : m_pParent(NULL), m_Index(0)
			{
			}

			T &operator *()
			{
				return m_pParent->m_Elements[m_Index];
			}
			const T &operator *() const
			{
				return m_pParent->m_Elements[m_Index];
			}
			
			T * operator->()
			{
				return m_pParent->m_Elements + m_Index;
			}

			const T * operator->() const
			{
				return m_pParent->m_Elements + m_Index;
			}

			iterator & operator++()		// preincrement
			{
				++m_Index;
				return (*this);
			}

			iterator operator++(int)	// postincrement
			{
				iterator tmp = *this;
				++m_Ptr;
				return tmp;
			}

			iterator & operator--()		// predecrement
			{
				--m_Index;
				return (*this);
			}

			iterator operator--(int)	// postdecrememnt
			{
				iterator tmp = *this;
				--m_Ptr;
				return tmp;
			}

			bool operator==(const iterator & right) const
			{
				return (m_pParent == right.m_pParent && m_Index == right.m_Index);
			}

			bool operator!=(const iterator & right) const
			{
				return !(*this == right);
			}
		};
		CStack() : m_Elements(new T[SH_STACK_DEFAULT_SIZE]),
			m_AllocatedSize(SH_STACK_DEFAULT_SIZE),
			m_UsedSize(0)
		{
			assert(m_Elements);
		}
		CStack(size_t size) : m_Elements(new T[size]),
			m_AllocatedSize(size),
			m_UsedSize(0)
		{
			assert(m_Elements);
		}

		~CStack()
		{
			if (m_Elements)
				delete [] m_Elements;
		}
		
		bool push(const T &val)
		{
			if (m_UsedSize + 1 == m_AllocatedSize)
			{
				// zOHNOES! REALLOCATE!
				m_AllocatedSize *= 2;
				T *newElements = new T[m_AllocatedSize];
				if (!newElements)
				{
					m_AllocatedSize /= 2;
					return false;
				}
				for (size_t i = 0; i < m_UsedSize; ++i)
					newElements[i] = m_Elements[i];
				delete [] m_Elements;
				m_Elements = newElements;
			}
			m_Elements[m_UsedSize++] = val;
			return true;
		}
		void pop()
		{
			--m_UsedSize;
		}

		T &front()
		{
			return m_Elements[m_UsedSize - 1];
		}

		const T &front() const
		{
			return m_Elements[m_UsedSize - 1];
		}

		iterator begin()
		{
			return iterator(this, 0);
		}
		iterator end()
		{
			return iterator(this, m_UsedSize);
		}
	};
};	//namespace SourceHook

#endif
