#include <string>
#include "sh_list.h"
#include "sh_stack.h"
#include "sh_tinyhash.h"
#include "sh_vector.h"
#include "testevents.h"

// TEST LIST
// Tests sh_list, sh_tinyhash, sh_vector

// :TODO: vector test, list insert test

namespace
{
	struct Hmm
	{
		Hmm *m_This;
		int m_Int;
		Hmm(const Hmm &other) : m_Int(other.m_Int)
		{
			m_This = this;
		}
		Hmm(int i) : m_Int(i)
		{
			m_This = this;
		}
		Hmm() : m_Int(0)
		{
			m_This = this;
		}
		void operator = (const Hmm &other)
		{
			m_Int = other.m_Int;
		}
		operator int () const
		{
			return m_Int;
		}
	};

	#define LIST_THIS_CHECK(lst, err) \
		for (ListType::iterator iter = lst.begin(); iter != lst.end(); ++iter) \
			CHECK_COND(&(*iter) == iter->m_This, err);

	bool DoTestList(std::string &error)
	{
		typedef SourceHook::List<Hmm> ListType;
		ListType lst;

		CHECK_COND(lst.empty(), "Part1");

		for (int i = 1; i <= 100; ++i)
			lst.push_back(i);

		LIST_THIS_CHECK(lst, "PartA1");

		CHECK_COND(!lst.empty(), "Part2");

		lst.clear();

		CHECK_COND(lst.empty(), "Part3");

		for (int i = 1; i <= 100; ++i)
			lst.push_back(i);

		CHECK_COND(lst.back() == 100, "Part4");
		LIST_THIS_CHECK(lst, "PartA2");

		int ver = 1;
		for (ListType::iterator iter = lst.begin(); iter != lst.end(); ++iter)
			CHECK_COND(*iter == ver++, "Part5");

		CHECK_COND(ver == 101, "Part 6");

		ListType::iterator iter50 = lst.find(50);
		CHECK_COND(*iter50 == 50, "Part7");

		iter50 = lst.erase(iter50);
		CHECK_COND(*iter50 == 51, "Part8");
		CHECK_COND(*--iter50 == 49, "Part8.2");

		lst.remove(80);

		ver = 1;
		for (ListType::iterator iter = lst.begin(); iter != lst.end(); ++iter)
		{
			CHECK_COND(*iter == ver++, "Part9");
			if (ver == 50 || ver == 80)
				++ver;
		}
		CHECK_COND(ver == 101, "Part10");
		LIST_THIS_CHECK(lst, "PartA3");

		ListType lst2;
		lst = lst2;
		CHECK_COND(lst.empty(), "Part11");

		for (int i = 1; i <= 100; ++i)
			lst.push_back(i);
		lst2 = lst;
		CHECK_COND(lst2.size() == 100, "Part11.2");

		LIST_THIS_CHECK(lst, "PartA4");
		LIST_THIS_CHECK(lst2, "PartA5");

		ver = 1;
		for (ListType::iterator iter = lst2.begin(); iter != lst2.end(); ++iter)
			CHECK_COND(*iter == ver++, "Part12");

		lst.clear();
		for (int i = 401; i <= 500; ++i)
			lst.push_back(i);
		lst = lst2;
		CHECK_COND(lst2.size() == 100, "Part13");

		ver = 1;
		for (ListType::iterator iter = lst.begin(); iter != lst.end(); ++iter)
			CHECK_COND(*iter == ver++, "Part14");

		LIST_THIS_CHECK(lst, "PartA6");
		LIST_THIS_CHECK(lst2, "PartA7");

		return true;
	}

	bool DoTestTinyHash(std::string &error)
	{
		const int mymax = 5000;

		typedef SourceHook::THash<int, int> HashType;
		HashType hash;

		for (int i = 1; i <= mymax; ++i)
			hash[i] = i + 5000;

		for (int i = 1; i <= mymax; ++i)
			CHECK_COND(hash[i] == i + 5000, "Part1");

		// Find
		int ver = 1;
		HashType::iterator iter;
		for (iter = hash.begin(); iter != hash.end(); ++iter)
			CHECK_COND(iter->key == ver && iter->val == (ver++) + 5000, "Part2");

		CHECK_COND(ver == mymax+1, "Part2.1");

		iter = hash.find(300);
		CHECK_COND(iter != hash.end() && iter->val == 300+5000, "Part3.1");
		iter = hash.find(mymax+200);
		CHECK_COND(iter == hash.end(), "Part3.2");

		HashType hash2;
		for (int i = 1; i <= mymax; ++i)
			hash2[i] = i + 5000;

		hash2.erase(mymax - 100);
		CHECK_COND(hash2.find(mymax - 101) != hash2.end(), "Part 4.1");
		CHECK_COND(hash2.find(mymax - 99) != hash2.end(), "Part 4.2");
		CHECK_COND(hash2.find(mymax - 100) == hash2.end(), "Part 4.3");
		hash2.erase(mymax - 99);
		CHECK_COND(hash2.find(mymax - 101) != hash2.end(), "Part 4.4");
		CHECK_COND(hash2.find(mymax - 99) == hash2.end(), "Part 4.5");
		CHECK_COND(hash2.find(mymax - 100) == hash2.end(), "Part 4.6");

		return true;
	}

	bool DoTestStack(std::string &error)
	{
		typedef SourceHook::CStack<int> IntStack;
		IntStack stk;
		int i;

		CHECK_COND(stk.size() == 0 && stk.empty(), "A0");

		for (i = 0; i < 5000; ++i)
			stk.push(i);

		CHECK_COND(stk.front() == 4999, "1");
		CHECK_COND(stk.size() == 5000 && !stk.empty(), "A1");

		IntStack::iterator iter;
		i = 0;
		for (iter = stk.begin(); iter != stk.end(); ++iter, ++i)
			CHECK_COND(*iter == i, "2");

		i = 0;
		for (iter = stk.begin(); iter != stk.end(); iter++, ++i)
			CHECK_COND(*iter == i, "3");

		--iter;
		iter--;
		*iter = 'g'+'a'+'b'+'e'+'n';
		stk.pop();

		CHECK_COND(stk.size() == 4999 && !stk.empty(), "A2");
		CHECK_COND(stk.front() == 'g'+'a'+'b'+'e'+'n', "4");
		
		IntStack stk2(stk);
		CHECK_COND(stk2.size() == 4999 && !stk2.empty(), "A3");

		IntStack::iterator iter2 = stk2.begin();
		for (iter = stk.begin(); iter != stk.end(); ++iter, iter2++)
			CHECK_COND(*iter == *iter2, "5");

		while (!stk2.empty())
			stk2.pop();
		CHECK_COND(stk2.size() == 0 && stk2.empty(), "A4");
		stk = stk2;
		CHECK_COND(stk.size() == 0 && stk.empty(), "A5");

		return true;
	}

	bool DoTestVec(std::string &error)
	{
		typedef SourceHook::CVector<int> IntVector;
		IntVector vec1;
		IntVector::iterator iter;
		int i;
		
		CHECK_COND(vec1.size() == 0 && vec1.empty(), "V1");
		for (i = 0; i < 500; ++i)
			vec1.push_back(i);

		CHECK_COND(vec1.size() == 500 && !vec1.empty(), "V2");

		for (i = 0; i < 500; ++i)
			CHECK_COND(vec1[i] == i, "V3");

		for (i = 0, iter = vec1.begin(); iter != vec1.end(); ++iter, ++i)
			CHECK_COND(*iter == i, "V4");

		vec1.resize(1000);
		for (i = 0; i < 500; ++i)
			CHECK_COND(vec1[i] == i, "V5.1");
		for (i = 500; i < 1000; ++i)
			CHECK_COND(vec1[i] == 0, "V5.2");

		vec1.resize(200);
		for (i = 0; i < 200; ++i)
			CHECK_COND(vec1[i] == i, "V6");

		vec1.resize(500, 0x12345678);
		for (i = 0; i < 200; ++i)
			CHECK_COND(vec1[i] == i, "V7.1");
		for (i = 200; i < 500; ++i)
			CHECK_COND(vec1[i] == 0x12345678, "V7.2");

		IntVector vec2(vec1);
		CHECK_COND(vec2.size() == vec1.size() && vec2.empty() == vec1.empty(), "V8.0");
		for (i = 0; i < 200; ++i)
			CHECK_COND(vec2[i] == i, "V8.1");
		for (i = 200; i < 500; ++i)
			CHECK_COND(vec2[i] == 0x12345678, "V8.2");


		vec1.clear();
		CHECK_COND(vec1.size() == 0 && vec1.empty() && vec1.begin() == vec1.end(), "V9");
		vec2 = vec1;
		CHECK_COND(vec2.size() == 0 && vec2.empty() && vec2.begin() == vec2.end(), "V9");

		return true;
	}
}

bool TestList(std::string &error)
{
	if (!DoTestList(error))
		return false;

	if (!DoTestTinyHash(error))
		return false;

	if (!DoTestStack(error))
		return false;

	if (!DoTestVec(error))
		return false;

	return true;
}
