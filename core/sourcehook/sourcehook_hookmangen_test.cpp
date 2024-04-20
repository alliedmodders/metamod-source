#include "sourcehook_pibuilder.h"
#include "sourcehook_hookmangen_test.h"

#include "metamod.h"

extern MetamodSource g_Metamod;

namespace SourceHook {
namespace Impl {
	SourceHook::IHookManagerAutoGen* g_pHookManager = nullptr;

	std::vector<SHT*> SHT::m_tests;

	void run_tests() {
		SH_DEBUG_LOG(TEST, "[SourceHook] Test Suite");

		g_pHookManager = static_cast<IHookManagerAutoGen*>(g_Metamod.MetaFactory(MMIFACE_SH_HOOKMANAUTOGEN, NULL, NULL));

		if (!g_SHPtr || !g_pHookManager) {
			SH_DEBUG_LOG(TEST, "SourceHook is not initialised!");
			return;
		}

		static std::uint32_t testIndex = 1;
		for (auto lambdas : SHT::m_tests) {
			try {
				lambdas->Call();
				SH_DEBUG_LOG(TEST, "TEST #%d \"%s\" - Passed", testIndex, lambdas->GetName().c_str());
			}
			catch (const SHTException& e) {
				SH_DEBUG_LOG(TEST, "TEST #%d \"%s\" - Failed\nError: \"%s\"", testIndex, lambdas->GetName().c_str(), e.what());
			}
			catch (const std::exception& e) {
				SH_DEBUG_LOG(TEST, "TEST #%d \"%s\" - Failed\nUnknown error: \"%s\"", testIndex, lambdas->GetName().c_str(), e.what());
			}
			catch (...) {
				SH_DEBUG_LOG(TEST, "TEST #%d \"%s\" - Unknown exception during test", testIndex, lambdas->GetName().c_str());
			}
			testIndex++;
		}

		SH_DEBUG_LOG(TEST, "[SourceHook] Test Suite - Ended");
	}

	template<typename ReturnType, typename... Args>
	class Function : public ISHDelegate {
	public:
		Function(CProtoInfoBuilder info, ReturnType (*virtualFunc)(void* thisPtr, Args... args), ReturnType (*hookFuncPre)(void* thisPtr, Args... args), ReturnType (*hookFuncPost)(void* thisPtr, Args... args)) :
			m_virtualFunc(virtualFunc),
			m_hookFuncPre(hookFuncPre),
			m_hookFuncPost(hookFuncPost),
			m_preCalled(false)
		{
			MemFuncInfo mfi = {false, -1, -1, -1};
			GetFuncInfo(&Function<ReturnType, Args...>::Test, mfi);

			if (!mfi.isVirtual || mfi.vtblindex <= 0) {
				throw SHTException("Failed to initialise test class");
			}

			m_hookMan = g_pHookManager->MakeHookMan(info, mfi.vtbloffs, mfi.vtblindex);
			m_hookIdPre = g_SHPtr->AddHook(g_PLID, ISourceHook::Hook_Normal, this, 0, m_hookMan, this, false);
			m_hookIdPost = g_SHPtr->AddHook(g_PLID, ISourceHook::Hook_Normal, this, 0, m_hookMan, this, true);

			SH_DEBUG_LOG(VERBOSE, "Setup Test Class %p - Vfnptr %p", this, (*(void***)this)[mfi.vtblindex]);
		}

	protected:
		virtual bool IsEqual(ISHDelegate *pOtherDeleg) override { return false; };
		virtual void DeleteThis() override { };
		virtual ReturnType Call(Args... args) {
			if (m_preCalled) {
				return (ReturnType)m_hookFuncPost(this, args...);
			}
			else {
				m_preCalled = true;
				return (ReturnType)m_hookFuncPre(this, args...);
			}
		};

		ReturnType (*m_virtualFunc)(void* thisPtr, Args... args);
		ReturnType (*m_hookFuncPre)(void* thisPtr, Args... args);
		ReturnType (*m_hookFuncPost)(void* thisPtr, Args... args);

		HookManagerPubFunc m_hookMan;
		int m_hookIdPre;
		int m_hookIdPost;
		bool m_preCalled;

	public:
		virtual ReturnType Test(Args... args) {
			return (ReturnType)m_virtualFunc(this, args...);
		};
	};

	void sht_assert(bool assert, const char* message) {
		if (!assert) {
			throw SHTException(message);
		}
	}

SHT test1("void_no_param", []() {
	static bool g_called = false;
	static bool g_pre_hook_called = false;
	static bool g_post_hook_called = false;
	static bool g_consistent_thisptr = true;
	static void* testThisPtr = nullptr;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, SourceHook::PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
		g_consistent_thisptr &= (thisPtr == testThisPtr);
	},
	[](void* thisPtr) {
		// Pre Hook
		g_pre_hook_called = true;
		g_consistent_thisptr &= (thisPtr == testThisPtr);
		RETURN_META(MRES_IGNORED);
	},
	[](void* thisPtr) {
		// Post hook
		g_post_hook_called = true;
		g_consistent_thisptr &= (thisPtr == testThisPtr);
		RETURN_META(MRES_IGNORED);
	});
	testThisPtr = test;

	test->Test();

	sht_assert(g_called, "Original function not called");
	sht_assert(g_pre_hook_called, "Pre hook not called");
	sht_assert(g_post_hook_called, "Post hook not called");
	sht_assert(g_consistent_thisptr, "Inconsistent this ptr across hooks");
});

SHT test2("void_one_int_param", []() {
	static bool g_called = false;
	static bool g_pre_hook_called = false;
	static bool g_post_hook_called = false;
	static bool g_number_consistent = true;
	static int g_test_number = 7680212;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.AddParam(sizeof(int), PassInfo::PassType_Basic, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);
	info.SetReturnType(0, PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void, int>(info,
	[](void* thisPtr, int param) {
		// Virtual Function
		g_called = true;
		g_number_consistent &= (param == g_test_number);
	},
	[](void* thisPtr, int param) {
		// Pre Hook
		g_pre_hook_called = true;
		g_number_consistent &= (param == g_test_number);
		RETURN_META(MRES_IGNORED);
	},
	[](void* thisPtr, int param) {
		// Post hook
		g_post_hook_called = true;
		g_number_consistent &= (param == g_test_number);
		RETURN_META(MRES_IGNORED);
	});

	test->Test(g_test_number);

	sht_assert(g_called, "Original function not called");
	sht_assert(g_pre_hook_called, "Pre hook not called");
	sht_assert(g_post_hook_called, "Post hook not called");
	sht_assert(g_number_consistent, "Int parameter changed");
});

SHT test3("void_mres_supercede", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META(MRES_SUPERCEDE);
	},
	[](void* thisPtr) {
		// Post hook
		RETURN_META(MRES_IGNORED);
	});

	test->Test();

	sht_assert(!g_called, "Original function was called");
});

SHT test4("return_mres_supercede", []() {
	static bool g_called = false;
	static bool g_valid_override = true;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(int), PassInfo::PassType_Basic, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<int>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
		return -1;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META_VALUE(MRES_SUPERCEDE, 0);
	},
	[](void* thisPtr) {
		// Post hook
		int value = *static_cast<const int*>(g_SHPtr->GetOverrideRet());
		g_valid_override &= (value == 0);
		RETURN_META_VALUE(MRES_IGNORED, 0);
	});

	g_valid_override &= (test->Test() == 0);

	sht_assert(!g_called, "Original function was called");
	sht_assert(g_valid_override, "Return value wasn't not overridden");
});

SHT test5("void_mres_override", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META(MRES_OVERRIDE);
	},
	[](void* thisPtr) {
		// Post hook
		RETURN_META(MRES_IGNORED);
	});

	test->Test();

	sht_assert(g_called, "Original function was not called");
});

SHT test6("void_mres_override", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META(MRES_IGNORED);
	},
	[](void* thisPtr) {
		// Post hook
		RETURN_META(MRES_OVERRIDE);
	});

	test->Test();

	sht_assert(g_called, "Original function was not called");
});

SHT test7("void_mres_supercede_2", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, PassInfo::PassType_Unknown, 0, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META(MRES_IGNORED);
	},
	[](void* thisPtr) {
		// Post hook
		RETURN_META(MRES_SUPERCEDE);
	});

	test->Test();

	sht_assert(g_called, "Original function was not called");
});

SHT test8("return_mres_supercede", []() {
	static bool g_called = false;
	static bool g_valid_override = true;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(int), PassInfo::PassType_Basic, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<int>(info,
	[](void* thisPtr) {
		// Virtual Function
		g_called = true;
		return -1;
	},
	[](void* thisPtr) {
		// Pre Hook
		RETURN_META_VALUE(MRES_OVERRIDE, 7);
	},
	[](void* thisPtr) {
		// Post hook
		int value = *static_cast<const int*>(g_SHPtr->GetOverrideRet());
		g_valid_override &= (value == 7);
		RETURN_META_VALUE(MRES_IGNORED, 5);
	});

	g_valid_override &= (test->Test() == 7);

	sht_assert(g_called, "Original function was not called");
	sht_assert(g_valid_override, "Return value wasn't not overridden");
});

SHT test9("return_vector_ref", []() {
	static bool g_called = false;
	static bool g_consistent_thisptr = true;
	static void* testThisPtr = nullptr;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByRef, nullptr, nullptr, nullptr, nullptr);

	static SDKVector originalVector(1, 2, 3);
	static SDKVector hookVector(4, 5, 6);

	auto test = new Function<SDKVector&>(info,
	[](void* thisPtr) -> SDKVector& {
		// Virtual Function
		g_called = true;
		g_consistent_thisptr &= (thisPtr == testThisPtr);
		return originalVector;
	},
	[](void* thisPtr) -> SDKVector& {
		// Pre Hook
		g_consistent_thisptr &= (thisPtr == testThisPtr);
		RETURN_META_VALUE(MRES_IGNORED, hookVector);
	},
	[](void* thisPtr) -> SDKVector& {
		// Post hook
		g_consistent_thisptr &= (thisPtr == testThisPtr);
		RETURN_META_VALUE(MRES_IGNORED, hookVector);
	});
	testThisPtr = test;

	auto& vector = test->Test();

	sht_assert(g_called, "Original function was not called");
	sht_assert(g_consistent_thisptr, "The this ptr was not consistent");
	sht_assert(&vector == &originalVector, "Original vector was not returned");
});

SHT test10("return_vector_ref_mres_supercede", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByRef, nullptr, nullptr, nullptr, nullptr);

	static SDKVector originalVector(1, 2, 3);
	static SDKVector hookVector(4, 5, 6);

	auto test = new Function<SDKVector&>(info,
	[](void* thisPtr) -> SDKVector& {
		// Virtual Function
		g_called = true;
		return originalVector;
	},
	[](void* thisPtr) -> SDKVector& {
		// Pre Hook
		RETURN_META_VALUE(MRES_SUPERCEDE, hookVector);
	},
	[](void* thisPtr) -> SDKVector& {
		// Post hook
		RETURN_META_VALUE(MRES_IGNORED, originalVector);
	});

	auto& vector = test->Test();

	sht_assert(!g_called, "Original function was called");
	sht_assert(&vector == &hookVector, "Hook vector was not returned");
});

SHT test11("return_vector_ref_mres_supercede_2", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByRef, nullptr, nullptr, nullptr, nullptr);

	static SDKVector originalVector(1, 2, 3);
	static SDKVector hookVector(4, 5, 6);

	auto test = new Function<SDKVector&>(info,
	[](void* thisPtr) -> SDKVector& {
		// Virtual Function
		g_called = true;
		return originalVector;
	},
	[](void* thisPtr) -> SDKVector& {
		// Pre Hook
		RETURN_META_VALUE(MRES_IGNORED, originalVector);
	},
	[](void* thisPtr) -> SDKVector& {
		// Post hook
		RETURN_META_VALUE(MRES_SUPERCEDE, hookVector);
	});

	auto& vector = test->Test();

	sht_assert(g_called, "Original function was not called");
	sht_assert(&vector == &hookVector, "Hook vector was not returned");
});

SHT test12("return_vector_ref_mres_override", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByRef, nullptr, nullptr, nullptr, nullptr);

	static SDKVector originalVector(1, 2, 3);
	static SDKVector hookVector(4, 5, 6);

	auto test = new Function<SDKVector&>(info,
	[](void* thisPtr) -> SDKVector& {
		// Virtual Function
		g_called = true;
		return originalVector;
	},
	[](void* thisPtr) -> SDKVector& {
		// Pre Hook
		RETURN_META_VALUE(MRES_OVERRIDE, hookVector);
	},
	[](void* thisPtr) -> SDKVector& {
		// Post hook
		RETURN_META_VALUE(MRES_IGNORED, originalVector);
	});

	auto& vector = test->Test();

	sht_assert(g_called, "Original function was not called");
	sht_assert(&vector == &hookVector, "Hook vector was not returned");
});

SHT test13("return_vector_mres_override", []() {
	static bool g_called = false;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);

	static SDKVector originalVector(1, 2, 3);
	static SDKVector hookVector(4, 5, 6);

	auto test = new Function<SDKVector>(info,
	[](void* thisPtr) -> SDKVector {
		// Virtual Function
		g_called = true;
		return originalVector;
	},
	[](void* thisPtr) -> SDKVector {
		// Pre Hook
		RETURN_META_VALUE(MRES_OVERRIDE, hookVector);
	},
	[](void* thisPtr) -> SDKVector {
		// Post hook
		RETURN_META_VALUE(MRES_IGNORED, originalVector);
	});

	auto vector = test->Test();

	sht_assert(g_called, "Original function was not called");
	sht_assert(vector.x == 4.0 && vector.y == 5.0 && vector.z == 6.0, "Hook vector was not returned");
});

SHT test14("void_three_params_vector_int_vector", []() {
	static bool g_called = false;
	static bool g_hook_pre_called = false;
	static bool g_hook_post_called = false;
	static bool g_valid_arg[4] = { true, true, true, true };
	static void* testThisPtr = nullptr;

	CProtoInfoBuilder info(ProtoInfo::CallConv_ThisCall);
	info.SetReturnType(0, PassInfo::PassType_Basic, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);
	info.AddParam(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);
	info.AddParam(sizeof(int), PassInfo::PassType_Basic, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);
	info.AddParam(sizeof(SDKVector), PassInfo::PassType_Object, PassInfo::PassFlag_ByVal, nullptr, nullptr, nullptr, nullptr);

	auto test = new Function<void, SDKVector, int, SDKVector>(info,
	[](void* thisPtr, SDKVector vec1, int arg2, SDKVector vec2) {
		// Virtual Function
		g_called = true;
		g_valid_arg[0] &= (vec1.x == 1.0 && vec1.y == 2.0 && vec1.z == 3.0);
		g_valid_arg[1] &= (arg2 == 777);
		g_valid_arg[2] &= (vec2.x == 4.0 && vec2.y == 5.0 && vec2.z == 6.0);
		g_valid_arg[3] &= (testThisPtr == thisPtr);
	},
	[](void* thisPtr, SDKVector vec1, int arg2, SDKVector vec2) {
		// Pre Hook
		g_valid_arg[0] &= (vec1.x == 1.0 && vec1.y == 2.0 && vec1.z == 3.0);
		g_valid_arg[1] &= (arg2 == 777);
		g_valid_arg[2] &= (vec2.x == 4.0 && vec2.y == 5.0 && vec2.z == 6.0);
		g_valid_arg[3] &= (testThisPtr == thisPtr);
		g_hook_pre_called = true;
		RETURN_META(MRES_IGNORED);
	},
	[](void* thisPtr, SDKVector vec1, int arg2, SDKVector vec2) {
		// Post hook
		g_valid_arg[0] &= (vec1.x == 1.0 && vec1.y == 2.0 && vec1.z == 3.0);
		g_valid_arg[1] &= (arg2 == 777);
		g_valid_arg[2] &= (vec2.x == 4.0 && vec2.y == 5.0 && vec2.z == 6.0);
		g_valid_arg[3] &= (testThisPtr == thisPtr);
		g_hook_post_called = true;
		RETURN_META(MRES_IGNORED);
	});
	testThisPtr = test;

	test->Test(SDKVector(1, 2, 3), 777, SDKVector(4, 5, 6));

	sht_assert(g_called, "Original function was not called");
	sht_assert(g_hook_pre_called, "Pre hook was not called");
	sht_assert(g_hook_pre_called, "Post hook was not called");
	sht_assert(g_valid_arg[3], "This ptr was not consistent");
	sht_assert(g_valid_arg[0] && g_valid_arg[1] && g_valid_arg[2], "Arguments were not valid");
});
}
}