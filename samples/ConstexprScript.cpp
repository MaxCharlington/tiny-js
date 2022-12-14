#include "TinyJS.hpp"
#include "TinyJS_Functions.hpp"

#include <cstdio>

consteval int run_JS(std::string_view code)
{
    CTinyJS js{};
    registerFunctions(&js);
    js.root->addChild("result", new CScriptVar("0", SCRIPTVAR_INTEGER));
    js.execute(code.data());
    return js.root->getParameter("result")->getInt();
}

int main()
{
    constexpr auto code = "function myfunc(x, y) { return x + y; } var a = myfunc(1,2); result = a;";
    constexpr int res = run_JS(code);
    static_assert(res == 3);
    printf("%d\n", res);
    return res;
}
