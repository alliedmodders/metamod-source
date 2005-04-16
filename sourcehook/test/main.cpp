// Hello BAIL!
// This is a test file

#include <stdio.h>
#include <iostream>
#include "sourcehook_impl.h"
#include "FastDelegate.h"

// Many base classes!
struct Base00
{
	int x0;
	virtual void LOL2(){
	}
};
struct Base0
{
	int x0;
	virtual void LOL(){
	}
};
struct Base1 : Base0, Base00
{
	int x;
	virtual void Func1(){}
};

struct Base2 : Base1
{
	int y;
	virtual void Func2(){}
};

struct Base3 : Base1
{
	int z;
	virtual void Func3(){}
};

// Now the main class follows!
// Why so many functions?
// So we can check the sh_memfuncinfo.h file's correctness
// F16 and F299 are actually used
// (I've written this by hand, it was fun)
class Test : public Base2, public Base3
{
public:
	virtual void F1(){}
	virtual void F2(){}
	virtual void F3(){}
	virtual void F4(){}
	virtual void F5(){}
	virtual void F6(){}
	virtual void F7(){}
	virtual void F8(){}
	virtual void F9(){}
	virtual void F10(){}
	virtual void F11(){}
	virtual void F12(){}
	virtual void F13(){}
	virtual void F14(){}
	virtual void F15(){}
	virtual bool F16(int x) { printf("Test::F16[0] called with x=%d; returning false\n", x); return false;}
	virtual bool F16(const char *x, ...)
	{
		char buf[4096];
		va_list argptr;
		va_start(argptr, x);
		vsprintf(buf, x, argptr);
		va_end(argptr);
		printf("Test::F16[1] called with o=%s; returning false\n", buf); return false;
	}
	virtual void F17(){}
	virtual void F18(){}
	virtual void F19(){}
	virtual void F20(){}
	virtual void F21(){}
	virtual void F22(){}
	virtual void F23(){}
	virtual void F24(){}
	virtual void F25(){}
	virtual void F26(){}
	virtual void F27(){}
	virtual void F28(){}
	virtual void F29(){}
	virtual void F30(){}
	virtual void F31(){}
	virtual void F32(){}
	virtual void F33(){}
	virtual void F34(){}
	virtual void F35(){}
	virtual void F36(){}
	virtual void F37(){}
	virtual void F38(){}
	virtual void F39(){}
	virtual void F40(){}
	virtual void F41(){}
	virtual void F42(){}
	virtual void F43(){}
	virtual void F44(){}
	virtual void F45(){}
	virtual void F46(){}
	virtual void F47(){}
	virtual void F48(){}
	virtual void F49(){}
	virtual void F50(){}
	virtual void F51(){}
	virtual void F52(){}
	virtual void F53(){}
	virtual void F54(){}
	virtual void F55(){}
	virtual void F56(){}
	virtual void F57(){}
	virtual void F58(){}
	virtual void F59(){}
	virtual void F60(){}
	virtual void F61(){}
	virtual void F62(){}
	virtual void F63(){}
	virtual void F64(){}
	virtual void F65(){}
	virtual void F66(){}
	virtual void F67(){}
	virtual void F68(){}
	virtual void F69(){}
	virtual void F70(){}
	virtual void F71(){}
	virtual void F72(){}
	virtual void F73(){}
	virtual void F74(){}
	virtual void F75(){}
	virtual void F76(){}
	virtual void F77(){}
	virtual void F78(){}
	virtual void F79(){}
	virtual void F80(){}
	virtual void F81(){}
	virtual void F82(){}
	virtual void F83(){}
	virtual void F84(){}
	virtual void F85(){}
	virtual void F86(){}
	virtual void F87(){}
	virtual void F88(){}
	virtual void F89(){}
	virtual void F90(){}
	virtual void F91(){}
	virtual void F92(){}
	virtual void F93(){}
	virtual void F94(){}
	virtual void F95(){}
	virtual void F96(){}
	virtual void F97(){}
	virtual void F98(){}
	virtual void F99(){}
	virtual void F100(){}
	virtual void F101(){}
	virtual void F102(){}
	virtual void F103(){}
	virtual void F104(){}
	virtual void F105(){}
	virtual void F106(){}
	virtual void F107(){}
	virtual void F108(){}
	virtual void F109(){}
	virtual void F110(){}
	virtual void F111(){}
	virtual void F112(){}
	virtual void F113(){}
	virtual void F114(){}
	virtual void F115(){}
	virtual void F116(){}
	virtual void F117(){}
	virtual void F118(){}
	virtual void F119(){}
	virtual void F120(){}
	virtual void F121(){}
	virtual void F122(){}
	virtual void F123(){}
	virtual void F124(){}
	virtual void F125(){}
	virtual void F126(){}
	virtual void F127(){}
	virtual void F128(){}
	virtual void F129(){}
	virtual void F130(){}
	virtual void F131(){}
	virtual void F132(){}
	virtual void F133(){}
	virtual void F134(){}
	virtual void F135(){}
	virtual void F136(){}
	virtual void F137(){}
	virtual void F138(){}
	virtual void F139(){}
	virtual void F140(){}
	virtual void F141(){}
	virtual void F142(){}
	virtual void F143(){}
	virtual void F144(){}
	virtual void F145(){}
	virtual void F146(){}
	virtual void F147(){}
	virtual void F148(){}
	virtual void F149(){}
	virtual void F150(){}
	virtual void F151(){}
	virtual void F152(){}
	virtual void F153(){}
	virtual void F154(){}
	virtual void F155(){}
	virtual void F156(){}
	virtual void F157(){}
	virtual void F158(){}
	virtual void F159(){}
	virtual void F160(){}
	virtual void F161(){}
	virtual void F162(){}
	virtual void F163(){}
	virtual void F164(){}
	virtual void F165(){}
	virtual void F166(){}
	virtual void F167(){}
	virtual void F168(){}
	virtual void F169(){}
	virtual void F170(){}
	virtual void F171(){}
	virtual void F172(){}
	virtual void F173(){}
	virtual void F174(){}
	virtual void F175(){}
	virtual void F176(){}
	virtual void F177(){}
	virtual void F178(){}
	virtual void F179(){}
	virtual void F180(){}
	virtual void F181(){}
	virtual void F182(){}
	virtual void F183(){}
	virtual void F184(){}
	virtual void F185(){}
	virtual void F186(){}
	virtual void F187(){}
	virtual void F188(){}
	virtual void F189(){}
	virtual void F190(){}
	virtual void F191(){}
	virtual void F192(){}
	virtual void F193(){}
	virtual void F194(){}
	virtual void F195(){}
	virtual void F196(){}
	virtual void F197(){}
	virtual void F198(){}
	virtual void F199(){}
	virtual void F200(){}
	virtual void F201(){}
	virtual void F202(){}
	virtual void F203(){}
	virtual void F204(){}
	virtual void F205(){}
	virtual void F206(){}
	virtual void F207(){}
	virtual void F208(){}
	virtual void F209(){}
	virtual void F210(){}
	virtual void F211(){}
	virtual void F212(){}
	virtual void F213(){}
	virtual void F214(){}
	virtual void F215(){}
	virtual void F216(){}
	virtual void F217(){}
	virtual void F218(){}
	virtual void F219(){}
	virtual void F220(){}
	virtual void F221(){}
	virtual void F222(){}
	virtual void F223(){}
	virtual void F224(){}
	virtual void F225(){}
	virtual void F226(){}
	virtual void F227(){}
	virtual void F228(){}
	virtual void F229(){}
	virtual void F230(){}
	virtual void F231(){}
	virtual void F232(){}
	virtual void F233(){}
	virtual void F234(){}
	virtual void F235(){}
	virtual void F236(){}
	virtual void F237(){}
	virtual void F238(){}
	virtual void F239(){}
	virtual void F240(){}
	virtual void F241(){}
	virtual void F242(){}
	virtual void F243(){}
	virtual void F244(){}
	virtual void F245(){}
	virtual void F246(){}
	virtual void F247(){}
	virtual void F248(){}
	virtual void F249(){}
	virtual void F250(){}
	virtual void F251(){}
	virtual void F252(){}
	virtual void F253(){}
	virtual void F254(){}
	virtual void F255(){}
	virtual void F256(){}
	virtual void F257(){}
	virtual void F258(){}
	virtual void F259(){}
	virtual void F260(){}
	virtual void F261(){}
	virtual void F262(){}
	virtual void F263(){}
	virtual void F264(){}
	virtual void F265(){}
	virtual void F266(){}
	virtual void F267(){}
	virtual void F268(){}
	virtual void F269(){}
	virtual void F270(){}
	virtual void F271(){}
	virtual void F272(){}
	virtual void F273(){}
	virtual void F274(){}
	virtual void F275(){}
	virtual void F276(){}
	virtual void F277(){}
	virtual void F278(){}
	virtual void F279(){}
	virtual void F280(){}
	virtual void F281(){}
	virtual void F282(){}
	virtual void F283(){}
	virtual void F284(){}
	virtual void F285(){}
	virtual void F286(){}
	virtual void F287(){}
	virtual void F288(){}
	virtual void F289(){}
	virtual void F290(){}
	virtual void F291(){}
	virtual void F292(){}
	virtual void F293(){}
	virtual void F294(){}
	virtual void F295(){}
	virtual void F296(){}
	virtual void F297(){}
	virtual void F298(){}
	virtual void F299(){ printf("Test::F299 was called. this=%p\n", this); }

	void G() {
	}
};

// Sourcehook needs g_Plug and g_SHPtr, it will be customizable in the next release
int g_PLID = 1337;
SourceHook::ISourceHook *g_SHPtr;

SourceHook::CSourceHookImpl g_SHImpl;

// You first have to declare the hook functions
SH_DECL_HOOK0_void(Test, F299, SH_NOATTRIB, 0);

// Test of overloaded functions
SH_DECL_HOOK1(Test, F16, SH_NOATTRIB, 0, bool, int);
SH_DECL_HOOK0_vafmt(Test, F16, SH_NOATTRIB, 1, bool);


// Handlers for F299
void Test_F299_pre_handler()
{
	printf("Pre hook handler for Test:F299 called. This=%p; ignoring\n", META_IFACEPTR);
}

void Test_F299_pre_handler2()
{
	printf("Pre hook handler for Test:F299 called. This=%p; superceding\n", META_IFACEPTR);
	RETURN_META(MRES_SUPERCEDE);
}

void Test_F299_post_handler()
{
	printf("Post hook handler for Test:F299 called. This=%p; ignoring\n", META_IFACEPTR);
}

// Handlers for F16
bool Test_F16_int_pre_handler(int x)
{
	printf("Pre hook handler for Test::F16 called. x=%d, this=%p; superceding with true\n", x, META_IFACEPTR);
	RETURN_META_VALUE(MRES_SUPERCEDE, true);
}

bool Test_F16_int_post_handler(int x)
{
	printf("Post hook handler for Test::F16(int) called. x=%d, this=%p, status=%d, orig ret=%d; overriding with !ret\n", x, META_IFACEPTR, META_RESULT_STATUS, META_RESULT_ORIG_RET(bool) ? 1 : 0);
	RETURN_META_VALUE(MRES_OVERRIDE, !META_RESULT_ORIG_RET(bool));
}

// I haven't checked the second F16 function yet

int main(int argc, char *argv[])
{
	// Get an instance and call the functions using the pointer
	// (otherwise the compiler optimizes away the vtable lookup)
	Test zLOL;
	Test *zLOL_Ptr = &zLOL;
	g_SHPtr = &g_SHImpl;

	//////////////////////////////////////////////////////////////////////////
	Test *cc = SH_GET_CALLCLASS(Test, zLOL_Ptr);

	printf("TEST1.1: Calling F299\n");
	zLOL_Ptr->F299();
	printf("TEST1.2: Calling F299 through cc\n");
	cc->F299();
	printf("TEST1.3: Hooking it\n");
	SH_ADD_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_pre_handler, false);
	printf("TEST1.4: Calling F299\n");
	zLOL_Ptr->F299();
	printf("TEST1.5: Calling F299 through cc\n");
	cc->F299();
	printf("TEST1.6: Adding one more pre and one post hook\n");
	SH_ADD_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_pre_handler2, false);
	SH_ADD_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_post_handler, true);
	printf("TEST1.7: Calling F299\n");
	zLOL_Ptr->F299();
	printf("TEST1.8: Calling F299 through cc\n");
	cc->F299();
	printf("TEST1.9: Removing pre hooks\n");
	SH_REMOVE_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_pre_handler, false);
	SH_REMOVE_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_pre_handler2, false);
	printf("TEST1.10: Calling F299\n");
	zLOL_Ptr->F299();
	printf("TEST1.11: Calling F299 through cc\n");
	cc->F299();
	printf("TEST1.12: Removing post hook\n");
	SH_REMOVE_HOOK_STATICFUNC(Test, F299, zLOL_Ptr, Test_F299_post_handler, true);
	printf("TEST1.13: Calling F299\n");
	zLOL_Ptr->F299();
	printf("TEST1.14: Calling F299 through cc\n");
	cc->F299();

	//////////////////////////////////////////////////////////////////////////
	printf("\n\n*************************************************\n\n");
	printf("TEST2.1: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.2: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.3: Hooking it\n");
	SH_ADD_HOOK_STATICFUNC(Test, F16, zLOL_Ptr, Test_F16_int_pre_handler, false);
	printf("TEST2.4: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.5: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.6: Adding post hook\n");
	SH_ADD_HOOK_STATICFUNC(Test, F16, zLOL_Ptr, Test_F16_int_post_handler, true);
	printf("TEST2.7: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.8: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.9: Removing pre hook\n");
	printf("TEST2.XX.1: Pausing the plugin\n");
	g_SHImpl.PausePlugin(g_PLID);
	printf("TEST2.XX.2: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.XX.3: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.XX.4: Unpausing the plugin\n");
	g_SHImpl.UnpausePlugin(g_PLID);
	printf("TEST2.XX.5: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.XX.6: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.9: Removing pre hook\n");
	SH_REMOVE_HOOK_STATICFUNC(Test, F16, zLOL_Ptr, Test_F16_int_pre_handler, false);
	printf("TEST2.10: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.11: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);
	printf("TEST2.12: Removing post hook\n");
	SH_REMOVE_HOOK_STATICFUNC(Test, F16, zLOL_Ptr, Test_F16_int_post_handler, true);
	printf("TEST2.10: Calling F16(155)\n");
	printf("Returned: %d\n", zLOL_Ptr->F16(155) ? 1 : 0);
	printf("TEST2.11: Calling F16(155) through CC\n");
	printf("Returned: %d\n", cc->F16(155) ? 1 : 0);

	SH_RELEASE_CALLCLASS(cc);

	// Type 0[ENTER] so that the program terminates
	int i;
	std::cin >> i;
}

// That is it, dear BAIL!