#include "TinyJS.hpp"
#include "TinyJS_Functions.hpp"

#include <array>
#include <cstdio>
#include <functional>
#include <cest/functional.hpp>
#include <vector>

class runtime
{
public:
    static constexpr std::size_t InstrsCnt = 256;

    constexpr runtime(const std::vector<void(*)()>& instrs)
        : instruction_count{std::min(InstrsCnt, instrs.size())}
    {
        for (std::size_t i = 0; i < instruction_count; i++)
            instructions[i] = instrs[i];
    }

    constexpr ~runtime() = default;

    int execute() const {
        for (std::size_t i = 0; i < instruction_count; i++)
            instructions[i]();
        return 0;
    }

private:
    std::array<void(*)(), InstrsCnt> instructions{};
    std::size_t instruction_count{InstrsCnt};
};

consteval auto run_JS(std::string_view code) -> runtime
{
    std::vector<void(*)()> instructions;
    CTinyJS js{};

    auto log = [](CScriptVar *v, void *) {
        // printf("> %s\n", v->getParameter("text")->getString().c_str());
    };
    js.addNative("function print(text)", log, 0);

    registerFunctions(&js);
    js.execute(code.data());
    return instructions;
}

int main()
{
    constexpr auto code = "function myfunc(x, y) { return x + y; } var a = myfunc(1,2);";
    constexpr auto r = run_JS(code);
    r.execute();
}
