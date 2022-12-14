/*
 * TinyJS
 *
 * A single-file Javascript-alike engine
 *
 * Authored By Gordon Williams <gw@pur3.co.uk>
 *
 * Copyright (C) 2009 Pur3 Ltd
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef TINYJS_HPP
#define TINYJS_HPP

// If defined, this keeps a note of all calls and where from in memory. This is slower, but good for debugging
#define TINYJS_CALL_STACK

#ifdef _WIN32
#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif
#endif
#include <string>
#include <vector>
#include <cassert>

#ifndef TRACE
#define TRACE printf
#endif // TRACE


const int TINYJS_LOOP_MAX_ITERATIONS = 8192;

// STDLIB
constexpr int stoi(const std::string& str)
{
    int ret = 0;

    if (str.length() == 0) return ret;

    bool negative = str[0] == '-';
    auto start_it = negative ? str.begin() + 1 : str.begin();

    for (auto num = start_it; num != str.end(); num++)
    {
        ret *= 10;
        ret += *num - '0';
    }

    if (negative) ret *= -1;

    return ret;
}

constexpr long stol(const std::string& str)
{
    long ret = 0;

    if (str.length() == 0) return ret;

    bool negative = str[0] == '-';
    auto start_it = negative ? str.begin() + 1 : str.begin();

    for (auto num = start_it; num != str.end(); num++)
    {
        ret *= 10;
        ret += *num - '0';
    }

    if (negative) ret *= -1;

    return ret;
}


enum LEX_TYPES {
    LEX_EOF = 0,
    LEX_ID = 256,
    LEX_INT,
    LEX_FLOAT,
    LEX_STR,

    LEX_EQUAL,
    LEX_TYPEEQUAL,
    LEX_NEQUAL,
    LEX_NTYPEEQUAL,
    LEX_LEQUAL,
    LEX_LSHIFT,
    LEX_LSHIFTEQUAL,
    LEX_GEQUAL,
    LEX_RSHIFT,
    LEX_RSHIFTUNSIGNED,
    LEX_RSHIFTEQUAL,
    LEX_PLUSEQUAL,
    LEX_MINUSEQUAL,
    LEX_PLUSPLUS,
    LEX_MINUSMINUS,
    LEX_ANDEQUAL,
    LEX_ANDAND,
    LEX_OREQUAL,
    LEX_OROR,
    LEX_XOREQUAL,
    // reserved words
#define LEX_R_LIST_START LEX_R_IF
    LEX_R_IF,
    LEX_R_ELSE,
    LEX_R_DO,
    LEX_R_WHILE,
    LEX_R_FOR,
    LEX_R_BREAK,
    LEX_R_CONTINUE,
    LEX_R_FUNCTION,
    LEX_R_RETURN,
    LEX_R_VAR,
    LEX_R_TRUE,
    LEX_R_FALSE,
    LEX_R_NULL,
    LEX_R_UNDEFINED,
    LEX_R_NEW,

	LEX_R_LIST_END /* always the last entry */
};

enum SCRIPTVAR_FLAGS {
    SCRIPTVAR_UNDEFINED   = 0,
    SCRIPTVAR_FUNCTION    = 1,
    SCRIPTVAR_OBJECT      = 2,
    SCRIPTVAR_ARRAY       = 4,
    SCRIPTVAR_DOUBLE      = 8,  // floating point double
    SCRIPTVAR_INTEGER     = 16, // integer number
    SCRIPTVAR_STRING      = 32, // string
    SCRIPTVAR_NULL        = 64, // it seems null is its own data type

    SCRIPTVAR_NATIVE      = 128, // to specify this is a native function
    SCRIPTVAR_NUMERICMASK = SCRIPTVAR_NULL |
                            SCRIPTVAR_DOUBLE |
                            SCRIPTVAR_INTEGER,
    SCRIPTVAR_VARTYPEMASK = SCRIPTVAR_DOUBLE |
                            SCRIPTVAR_INTEGER |
                            SCRIPTVAR_STRING |
                            SCRIPTVAR_FUNCTION |
                            SCRIPTVAR_OBJECT |
                            SCRIPTVAR_ARRAY |
                            SCRIPTVAR_NULL,

};

#define TINYJS_RETURN_VAR "return"
#define TINYJS_PROTOTYPE_CLASS "prototype"
#define TINYJS_TEMP_NAME ""
#define TINYJS_BLANK_DATA ""

/// convert the given string into a quoted string suitable for javascript
constexpr std::string getJSString(const std::string &str);

class CScriptException {
public:
    std::string text;
    constexpr CScriptException(const std::string &exceptionText);
};

class CScriptLex
{
public:
    constexpr CScriptLex(const std::string &input);
    constexpr CScriptLex(CScriptLex *owner, int startChar, int endChar);

    char currCh{};
    char nextCh{};
    int tk; ///< The type of the token that we have
    int tokenStart; ///< Position in the data at the beginning of the token we have here
    int tokenEnd; ///< Position in the data at the last character of the token we have here
    int tokenLastEnd; ///< Position in the data at the last character of the last token
    std::string tkStr; ///< Data contained in the token we have here

    constexpr void match(int expected_tk); ///< Lexical match wotsit
    constexpr static std::string getTokenStr(int token); ///< Get the string representation of the given token
    constexpr void reset(); ///< Reset this lex so we can start again

    constexpr std::string getSubString(int pos); ///< Return a sub-string from the given position up until right now
    constexpr CScriptLex *getSubLex(int lastPosition); ///< Return a sub-lexer from the given position up until right now

    constexpr std::string getPosition(int pos=-1); ///< Return a string representing the position in lines and columns of the character pos given

protected:
    /* When we go into a loop, we use getSubLex to get a lexer for just the sub-part of the
       relevant string. This doesn't re-allocate and copy the string, but instead copies
       the data pointer and sets dataOwned to false, and dataStart/dataEnd to the relevant things. */
    std::string data; ///< Data string to get tokens from
    std::size_t dataStart, dataEnd; ///< Start and end position in data string
    bool dataOwned; ///< Do we own this data string?

    std::size_t dataPos; ///< Position in data (we CAN go past the end of the string here)

    constexpr void getNextCh();
    constexpr void getNextToken(); ///< Get the text token from our text string
};

class CScriptVar;

using JSCallback = void (*)(CScriptVar *var, void *userdata);

class CScriptVarLink
{
public:
  std::string name;
  CScriptVarLink *nextSibling;
  CScriptVarLink *prevSibling;
  CScriptVar *var;
  bool owned;

  constexpr CScriptVarLink(CScriptVar *var, const std::string &name = TINYJS_TEMP_NAME);
  constexpr CScriptVarLink(const CScriptVarLink &link); ///< Copy constructor
  constexpr ~CScriptVarLink();
  constexpr void replaceWith(CScriptVar *newVar); ///< Replace the Variable pointed to
  constexpr void replaceWith(CScriptVarLink *newVar); ///< Replace the Variable pointed to (just dereferences)
  constexpr int getIntName(); ///< Get the name as an integer (for arrays)
  constexpr void setIntName(int n); ///< Set the name as an integer (for arrays)
};

/// Variable class (containing a doubly-linked list of children)
class CScriptVar
{
public:
    constexpr CScriptVar(); ///< Create undefined
    constexpr CScriptVar(const std::string &varData, int varFlags); ///< User defined
    constexpr CScriptVar(const std::string &str); ///< Create a string
    constexpr CScriptVar(double varData);
    constexpr CScriptVar(int val);
    constexpr ~CScriptVar();

    constexpr CScriptVar *getReturnVar(); ///< If this is a function, get the result value (for use by native functions)
    constexpr void setReturnVar(CScriptVar *var); ///< Set the result value. Use this when setting complex return data as it avoids a deepCopy()
    constexpr CScriptVar *getParameter(const std::string &name); ///< If this is a function, get the parameter with the given name (for use by native functions)

    constexpr CScriptVarLink *findChild(const std::string &childName); ///< Tries to find a child with the given name, may return 0
    constexpr CScriptVarLink *findChildOrCreate(const std::string &childName, int varFlags=SCRIPTVAR_UNDEFINED); ///< Tries to find a child with the given name, or will create it with the given flags
    constexpr CScriptVarLink *findChildOrCreateByPath(const std::string &path); ///< Tries to find a child with the given path (separated by dots)
    constexpr CScriptVarLink *addChild(const std::string &childName, CScriptVar *child=NULL);
    constexpr CScriptVarLink *addChildNoDup(const std::string &childName, CScriptVar *child=NULL); ///< add a child overwriting any with the same name
    constexpr void removeChild(CScriptVar *child);
    constexpr void removeLink(CScriptVarLink *link); ///< Remove a specific link (this is faster than finding via a child)
    constexpr void removeAllChildren();
    constexpr CScriptVar *getArrayIndex(int idx); ///< The the value at an array index
    constexpr void setArrayIndex(int idx, CScriptVar *value); ///< Set the value at an array index
    constexpr int getArrayLength(); ///< If this is an array, return the number of items in it (else 0)
    constexpr int getChildren(); ///< Get the number of children

    constexpr int getInt();
    constexpr bool getBool() { return getInt() != 0; }
    constexpr double getDouble();
    constexpr const std::string &getString();
    constexpr std::string getParsableString(); ///< get Data as a parsable javascript string
    constexpr void setInt(int num);
    constexpr void setDouble(double val);
    constexpr void setString(const std::string &str);
    constexpr void setUndefined();
    constexpr void setArray();
    constexpr bool equals(CScriptVar *v);

    constexpr bool isInt() { return (flags&SCRIPTVAR_INTEGER)!=0; }
    constexpr bool isDouble() { return (flags&SCRIPTVAR_DOUBLE)!=0; }
    constexpr bool isString() { return (flags&SCRIPTVAR_STRING)!=0; }
    constexpr bool isNumeric() { return (flags&SCRIPTVAR_NUMERICMASK)!=0; }
    constexpr bool isFunction() { return (flags&SCRIPTVAR_FUNCTION)!=0; }
    constexpr bool isObject() { return (flags&SCRIPTVAR_OBJECT)!=0; }
    constexpr bool isArray() { return (flags&SCRIPTVAR_ARRAY)!=0; }
    constexpr bool isNative() { return (flags&SCRIPTVAR_NATIVE)!=0; }
    constexpr bool isUndefined() { return (flags & SCRIPTVAR_VARTYPEMASK) == SCRIPTVAR_UNDEFINED; }
    constexpr bool isNull() { return (flags & SCRIPTVAR_NULL)!=0; }
    constexpr bool isBasic() { return firstChild==0; } ///< Is this *not* an array/object/etc

    constexpr CScriptVar *mathsOp(CScriptVar *b, int op); ///< do a maths op with another script variable
    constexpr void copyValue(CScriptVar *val); ///< copy the value from the value given
    constexpr CScriptVar *deepCopy(); ///< deep copy this node and return the result

    constexpr void trace(std::string indentStr = "", const std::string &name = ""); ///< Dump out the contents of this using trace
    constexpr std::string getFlagsAsString(); ///< For debugging - just dump a string version of the flags
    constexpr  std::string getJSON(const std::string linePrefix=""); ///< Write out all the JS code needed to recreate this script variable to the stream (as JSON)
    constexpr void setCallback(JSCallback callback, void *userdata); ///< Set the callback for native functions

    CScriptVarLink *firstChild;
    CScriptVarLink *lastChild;

    /// For memory management/garbage collection
    constexpr CScriptVar *ref(); ///< Add reference to this variable
    constexpr void unref(); ///< Remove a reference, and delete this variable if required
    constexpr int getRefs(); ///< Get the number of references to this script variable
protected:
    int refs; ///< The number of references held to this - used for garbage collection

    std::string data; ///< The contents of this variable if it is a string
    long intData; ///< The contents of this variable if it is an int
    double doubleData; ///< The contents of this variable if it is a double
    int flags; ///< the flags determine the type of the variable - int/double/string/etc
    JSCallback jsCallback; ///< Callback for native functions
    void *jsCallbackUserData; ///< user data passed as second argument to native functions

    constexpr void init(); ///< initialisation of data members

    /** Copy the basic data and flags from the variable given, with no
      * children. Should be used internally only - by copyValue and deepCopy */
    constexpr void copySimpleData(CScriptVar *val);

    friend class CTinyJS;
};

class CTinyJS {
public:
    constexpr CTinyJS();
    constexpr ~CTinyJS();

    constexpr void execute(const std::string &code);
    /** Evaluate the given code and return a link to a javascript object,
     * useful for (dangerous) JSON parsing. If nothing to return, will return
     * 'undefined' variable type. CScriptVarLink is returned as this will
     * automatically unref the result as it goes out of scope. If you want to
     * keep it, you must use ref() and unref() */
    constexpr CScriptVarLink evaluateComplex(const std::string &code);
    /** Evaluate the given code and return a string. If nothing to return, will return
     * 'undefined' */
    constexpr std::string evaluate(const std::string &code);

    /// add a native function to be called from TinyJS
    /** example:
       \code
           void scRandInt(CScriptVar *c, void *userdata) { ... }
           tinyJS->addNative("function randInt(min, max)", scRandInt, 0);
       \endcode

       or

       \code
           void scSubstring(CScriptVar *c, void *userdata) { ... }
           tinyJS->addNative("function String.substring(lo, hi)", scSubstring, 0);
       \endcode
    */
    constexpr void addNative(const std::string &funcDesc, JSCallback ptr, void *userdata);

    /// Get the given variable specified by a path (var1.var2.etc), or return 0
    constexpr CScriptVar *getScriptVariable(const std::string &path);
    /// Get the value of the given variable, or return 0
    constexpr const std::string *getVariable(const std::string &path);
    /// set the value of the given variable, return trur if it exists and gets set
    constexpr bool setVariable(const std::string &path, const std::string &varData);

    /// Send all variables to stdout
    constexpr void trace();

    CScriptVar *root{nullptr};   /// root of symbol table
private:
    CScriptLex *l{nullptr};             /// current lexer
    std::vector<CScriptVar*> scopes; /// stack of scopes when parsing
#ifdef TINYJS_CALL_STACK
    std::vector<std::string> call_stack; /// Names of places called so we can show when erroring
#endif

    CScriptVar *stringClass{nullptr}; /// Built in string class
    CScriptVar *objectClass{nullptr}; /// Built in object class
    CScriptVar *arrayClass{nullptr}; /// Built in array class

    // parsing - in order of precedence
    constexpr CScriptVarLink *functionCall(bool &execute, CScriptVarLink *function, CScriptVar *parent);
    constexpr CScriptVarLink *factor(bool &execute);
    constexpr CScriptVarLink *unary(bool &execute);
    constexpr CScriptVarLink *term(bool &execute);
    constexpr CScriptVarLink *expression(bool &execute);
    constexpr CScriptVarLink *shift(bool &execute);
    constexpr CScriptVarLink *condition(bool &execute);
    constexpr CScriptVarLink *logic(bool &execute);
    constexpr CScriptVarLink *ternary(bool &execute);
    constexpr CScriptVarLink *base(bool &execute);
    constexpr void block(bool &execute);
    constexpr void statement(bool &execute);
    // parsing utility functions
    constexpr CScriptVarLink *parseFunctionDefinition();
    constexpr void parseFunctionArguments(CScriptVar *funcVar);

    constexpr CScriptVarLink *findInScopes(const std::string &childName); ///< Finds a child, looking recursively up the scopes
    /// Look up in any parent classes of the given object
    constexpr CScriptVarLink *findInParentClasses(CScriptVar *object, const std::string &name);
};

#define ASSERT(X) assert(X)
/* Frees the given link IF it isn't owned by anything else */
#define CLEAN(x) { CScriptVarLink *__v = x; if (__v && !__v->owned) { delete __v; } }
/* Create a LINK to point to VAR and free the old link.
 * BUT this is more clever - it tries to keep the old link if it's not owned to save allocations */
#define CREATE_LINK(LINK, VAR) { if (!LINK || LINK->owned) LINK = new CScriptVarLink(VAR); else LINK->replaceWith(VAR); }

#include <string>
#include <string.h>
#include <sstream>
#include <cstdlib>
#include <stdio.h>

using namespace std;

#ifdef _WIN32
#ifdef _DEBUG
   #ifndef DBG_NEW
      #define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )
      #define new DBG_NEW
   #endif
#endif
#endif

#ifdef __GNUC__
#define vsprintf_s vsnprintf
#define sprintf_s snprintf
#define _strdup strdup
#endif

// ----------------------------------------------------------------------------------- Memory Debug

#define DEBUG_MEMORY 0

#if DEBUG_MEMORY

vector<CScriptVar*> allocatedVars;
vector<CScriptVarLink*> allocatedLinks;

void mark_allocated(CScriptVar *v) {
    allocatedVars.push_back(v);
}

void mark_deallocated(CScriptVar *v) {
    for (size_t i=0;i<allocatedVars.size();i++) {
      if (allocatedVars[i] == v) {
        allocatedVars.erase(allocatedVars.begin()+i);
        break;
      }
    }
}

void mark_allocated(CScriptVarLink *v) {
    allocatedLinks.push_back(v);
}

void mark_deallocated(CScriptVarLink *v) {
    for (size_t i=0;i<allocatedLinks.size();i++) {
      if (allocatedLinks[i] == v) {
        allocatedLinks.erase(allocatedLinks.begin()+i);
        break;
      }
    }
}

void show_allocated() {
    for (size_t i=0;i<allocatedVars.size();i++) {
      printf("ALLOCATED, %d refs\n", allocatedVars[i]->getRefs());
      allocatedVars[i]->trace("  ");
    }
    for (size_t i=0;i<allocatedLinks.size();i++) {
      printf("ALLOCATED LINK %s, allocated[%d] to \n", allocatedLinks[i]->name.c_str(), allocatedLinks[i]->var->getRefs());
      allocatedLinks[i]->var->trace("  ");
    }
    allocatedVars.clear();
    allocatedLinks.clear();
}
#endif

// ----------------------------------------------------------------------------------- Utils
constexpr bool isWhitespace(char ch) {
    return (ch==' ') || (ch=='\t') || (ch=='\n') || (ch=='\r');
}

constexpr bool isNumeric(char ch) {
    return (ch>='0') && (ch<='9');
}

constexpr bool isNumber(const string &str) {
    for (size_t i=0;i<str.size();i++)
      if (!isNumeric(str[i])) return false;
    return true;
}

constexpr bool isHexadecimal(char ch) {
    return ((ch>='0') && (ch<='9')) ||
           ((ch>='a') && (ch<='f')) ||
           ((ch>='A') && (ch<='F'));
}

constexpr bool isAlpha(char ch) {
    return ((ch>='a') && (ch<='z')) || ((ch>='A') && (ch<='Z')) || ch=='_';
}

constexpr bool isIDString(const char *s) {
    if (!isAlpha(*s))
        return false;
    while (*s) {
        if (!(isAlpha(*s) || isNumeric(*s)))
            return false;
        s++;
    }
    return true;
}

constexpr void replace(string &str, char textFrom, const char *textTo) {
    size_t sLen = strlen(textTo);
    size_t p = str.find(textFrom);
    while (p != string::npos) {
        str = str.substr(0, p) + textTo + str.substr(p+1);
        p = str.find(textFrom, p+sLen);
    }
}

/// convert the given string into a quoted string suitable for javascript
constexpr std::string getJSString(const std::string &str) {
    std::string nStr = str;
    for (size_t i=0;i<nStr.size();i++) {
      const char *replaceWith = "";
      bool replace = true;

      switch (nStr[i]) {
        case '\\': replaceWith = "\\\\"; break;
        case '\n': replaceWith = "\\n"; break;
        case '\r': replaceWith = "\\r"; break;
        case '\a': replaceWith = "\\a"; break;
        case '"': replaceWith = "\\\""; break;
        default: {
          int nCh = ((int)nStr[i]) &0xFF;
          if (nCh<32 || nCh>127) {
            char buffer[5];
            sprintf_s(buffer, 5, "\\x%02X", nCh);
            replaceWith = buffer;
          } else replace=false;
        }
      }

      if (replace) {
        nStr = nStr.substr(0, i) + replaceWith + nStr.substr(i+1);
        i += strlen(replaceWith)-1;
      }
    }
    return "\"" + nStr + "\"";
}

/** Is the string alphanumeric */
constexpr bool isAlphaNum(const std::string &str) {
    if (str.size()==0) return true;
    if (!isAlpha(str[0])) return false;
    for (size_t i=0;i<str.size();i++)
      if (!(isAlpha(str[i]) || isNumeric(str[i])))
        return false;
    return true;
}

// ----------------------------------------------------------------------------------- CSCRIPTEXCEPTION

constexpr CScriptException::CScriptException(const string &exceptionText) {
    text = exceptionText;
}

// ----------------------------------------------------------------------------------- CSCRIPTLEX

constexpr CScriptLex::CScriptLex(const string &input) {
    data = input;
    dataStart = 0;
    dataEnd = data.length();
    reset();
}

constexpr CScriptLex::CScriptLex(CScriptLex *owner, int startChar, int endChar) {
    data = owner->data;
    dataStart = startChar;
    dataEnd = endChar;
    reset();
}

constexpr void CScriptLex::reset() {
    dataPos = dataStart;
    tokenStart = 0;
    tokenEnd = 0;
    tokenLastEnd = 0;
    tk = 0;
    tkStr = "";
    getNextCh();
    getNextCh();
    getNextToken();
}

constexpr void CScriptLex::match(int expected_tk) {
    if (tk!=expected_tk) {
        ostringstream errorString;
        errorString << "Got " << getTokenStr(tk) << " expected " << getTokenStr(expected_tk)
         << " at " << getPosition(tokenStart);
        throw new CScriptException(errorString.str());
    }
    getNextToken();
}

constexpr string CScriptLex::getTokenStr(int token) {
    if (token>32 && token<128) {
        char buf[4] = "' '";
        buf[1] = (char)token;
        return buf;
    }
    switch (token) {
        case LEX_EOF : return "EOF";
        case LEX_ID : return "ID";
        case LEX_INT : return "INT";
        case LEX_FLOAT : return "FLOAT";
        case LEX_STR : return "STRING";
        case LEX_EQUAL : return "==";
        case LEX_TYPEEQUAL : return "===";
        case LEX_NEQUAL : return "!=";
        case LEX_NTYPEEQUAL : return "!==";
        case LEX_LEQUAL : return "<=";
        case LEX_LSHIFT : return "<<";
        case LEX_LSHIFTEQUAL : return "<<=";
        case LEX_GEQUAL : return ">=";
        case LEX_RSHIFT : return ">>";
        case LEX_RSHIFTUNSIGNED : return ">>";
        case LEX_RSHIFTEQUAL : return ">>=";
        case LEX_PLUSEQUAL : return "+=";
        case LEX_MINUSEQUAL : return "-=";
        case LEX_PLUSPLUS : return "++";
        case LEX_MINUSMINUS : return "--";
        case LEX_ANDEQUAL : return "&=";
        case LEX_ANDAND : return "&&";
        case LEX_OREQUAL : return "|=";
        case LEX_OROR : return "||";
        case LEX_XOREQUAL : return "^=";
                // reserved words
        case LEX_R_IF : return "if";
        case LEX_R_ELSE : return "else";
        case LEX_R_DO : return "do";
        case LEX_R_WHILE : return "while";
        case LEX_R_FOR : return "for";
        case LEX_R_BREAK : return "break";
        case LEX_R_CONTINUE : return "continue";
        case LEX_R_FUNCTION : return "function";
        case LEX_R_RETURN : return "return";
        case LEX_R_VAR : return "var";
        case LEX_R_TRUE : return "true";
        case LEX_R_FALSE : return "false";
        case LEX_R_NULL : return "null";
        case LEX_R_UNDEFINED : return "undefined";
        case LEX_R_NEW : return "new";
    }

    ostringstream msg;
    msg << "?[" << token << "]";
    return msg.str();
}

constexpr void CScriptLex::getNextCh() {
    currCh = nextCh;
    if (dataPos < dataEnd)
        nextCh = data[dataPos];
    else
        nextCh = 0;
    dataPos++;
}

constexpr void CScriptLex::getNextToken() {
    tk = LEX_EOF;
    tkStr.clear();
    while (currCh && isWhitespace(currCh)) getNextCh();
    // newline comments
    if (currCh=='/' && nextCh=='/') {
        while (currCh && currCh!='\n') getNextCh();
        getNextCh();
        getNextToken();
        return;
    }
    // block comments
    if (currCh=='/' && nextCh=='*') {
        while (currCh && (currCh!='*' || nextCh!='/')) getNextCh();
        getNextCh();
        getNextCh();
        getNextToken();
        return;
    }
    // record beginning of this token
    tokenStart = dataPos-2;
    // tokens
    if (isAlpha(currCh)) { //  IDs
        while (isAlpha(currCh) || isNumeric(currCh)) {
            tkStr += currCh;
            getNextCh();
        }
        tk = LEX_ID;
             if (tkStr=="if") tk = LEX_R_IF;
        else if (tkStr=="else") tk = LEX_R_ELSE;
        else if (tkStr=="do") tk = LEX_R_DO;
        else if (tkStr=="while") tk = LEX_R_WHILE;
        else if (tkStr=="for") tk = LEX_R_FOR;
        else if (tkStr=="break") tk = LEX_R_BREAK;
        else if (tkStr=="continue") tk = LEX_R_CONTINUE;
        else if (tkStr=="function") tk = LEX_R_FUNCTION;
        else if (tkStr=="return") tk = LEX_R_RETURN;
        else if (tkStr=="var") tk = LEX_R_VAR;
        else if (tkStr=="true") tk = LEX_R_TRUE;
        else if (tkStr=="false") tk = LEX_R_FALSE;
        else if (tkStr=="null") tk = LEX_R_NULL;
        else if (tkStr=="undefined") tk = LEX_R_UNDEFINED;
        else if (tkStr=="new") tk = LEX_R_NEW;
    } else if (isNumeric(currCh)) { // Numbers
        bool isHex = false;
        if (currCh=='0') { tkStr += currCh; getNextCh(); }
        if (currCh=='x') {
          isHex = true;
          tkStr += currCh; getNextCh();
        }
        tk = LEX_INT;
        while (isNumeric(currCh) || (isHex && isHexadecimal(currCh))) {
            tkStr += currCh;
            getNextCh();
        }
        if (!isHex && currCh=='.') {
            tk = LEX_FLOAT;
            tkStr += '.';
            getNextCh();
            while (isNumeric(currCh)) {
                tkStr += currCh;
                getNextCh();
            }
        }
        // do fancy e-style floating point
        if (!isHex && (currCh=='e'||currCh=='E')) {
          tk = LEX_FLOAT;
          tkStr += currCh; getNextCh();
          if (currCh=='-') { tkStr += currCh; getNextCh(); }
          while (isNumeric(currCh)) {
             tkStr += currCh; getNextCh();
          }
        }
    } else if (currCh=='"') {
        // strings...
        getNextCh();
        while (currCh && currCh!='"') {
            if (currCh == '\\') {
                getNextCh();
                switch (currCh) {
                case 'n' : tkStr += '\n'; break;
                case '"' : tkStr += '"'; break;
                case '\\' : tkStr += '\\'; break;
                default: tkStr += currCh;
                }
            } else {
                tkStr += currCh;
            }
            getNextCh();
        }
        getNextCh();
        tk = LEX_STR;
    } else if (currCh=='\'') {
        // strings again...
        getNextCh();
        while (currCh && currCh!='\'') {
            if (currCh == '\\') {
                getNextCh();
                switch (currCh) {
                case 'n' : tkStr += '\n'; break;
                case 'a' : tkStr += '\a'; break;
                case 'r' : tkStr += '\r'; break;
                case 't' : tkStr += '\t'; break;
                case '\'' : tkStr += '\''; break;
                case '\\' : tkStr += '\\'; break;
                case 'x' : { // hex digits
                              char buf[3] = "??";
                              getNextCh(); buf[0] = currCh;
                              getNextCh(); buf[1] = currCh;
                              tkStr += (char)strtol(buf,0,16);
                           } break;
                default: if (currCh>='0' && currCh<='7') {
                           // octal digits
                           char buf[4] = "???";
                           buf[0] = currCh;
                           getNextCh(); buf[1] = currCh;
                           getNextCh(); buf[2] = currCh;
                           tkStr += (char)strtol(buf,0,8);
                         } else
                           tkStr += currCh;
                }
            } else {
                tkStr += currCh;
            }
            getNextCh();
        }
        getNextCh();
        tk = LEX_STR;
    } else {
        // single chars
        tk = currCh;
        if (currCh) getNextCh();
        if (tk=='=' && currCh=='=') { // ==
            tk = LEX_EQUAL;
            getNextCh();
            if (currCh=='=') { // ===
              tk = LEX_TYPEEQUAL;
              getNextCh();
            }
        } else if (tk=='!' && currCh=='=') { // !=
            tk = LEX_NEQUAL;
            getNextCh();
            if (currCh=='=') { // !==
              tk = LEX_NTYPEEQUAL;
              getNextCh();
            }
        } else if (tk=='<' && currCh=='=') {
            tk = LEX_LEQUAL;
            getNextCh();
        } else if (tk=='<' && currCh=='<') {
            tk = LEX_LSHIFT;
            getNextCh();
            if (currCh=='=') { // <<=
              tk = LEX_LSHIFTEQUAL;
              getNextCh();
            }
        } else if (tk=='>' && currCh=='=') {
            tk = LEX_GEQUAL;
            getNextCh();
        } else if (tk=='>' && currCh=='>') {
            tk = LEX_RSHIFT;
            getNextCh();
            if (currCh=='=') { // >>=
              tk = LEX_RSHIFTEQUAL;
              getNextCh();
            } else if (currCh=='>') { // >>>
              tk = LEX_RSHIFTUNSIGNED;
              getNextCh();
            }
        }  else if (tk=='+' && currCh=='=') {
            tk = LEX_PLUSEQUAL;
            getNextCh();
        }  else if (tk=='-' && currCh=='=') {
            tk = LEX_MINUSEQUAL;
            getNextCh();
        }  else if (tk=='+' && currCh=='+') {
            tk = LEX_PLUSPLUS;
            getNextCh();
        }  else if (tk=='-' && currCh=='-') {
            tk = LEX_MINUSMINUS;
            getNextCh();
        } else if (tk=='&' && currCh=='=') {
            tk = LEX_ANDEQUAL;
            getNextCh();
        } else if (tk=='&' && currCh=='&') {
            tk = LEX_ANDAND;
            getNextCh();
        } else if (tk=='|' && currCh=='=') {
            tk = LEX_OREQUAL;
            getNextCh();
        } else if (tk=='|' && currCh=='|') {
            tk = LEX_OROR;
            getNextCh();
        } else if (tk=='^' && currCh=='=') {
            tk = LEX_XOREQUAL;
            getNextCh();
        }
    }
    /* This isn't quite right yet */
    tokenLastEnd = tokenEnd;
    tokenEnd = dataPos-3;
}

constexpr string CScriptLex::getSubString(int lastPosition) {
    int lastCharIdx = tokenLastEnd+1;
    if (lastCharIdx < dataEnd) {
        /* save a memory alloc by using our data array to create the
           substring */
        char old = data[lastCharIdx];
        data[lastCharIdx] = 0;
        string value = &data[lastPosition];
        data[lastCharIdx] = old;
        return value;
    } else {
        return string(&data[lastPosition]);
    }
}


constexpr CScriptLex *CScriptLex::getSubLex(int lastPosition) {
    int lastCharIdx = tokenLastEnd+1;
    if (lastCharIdx < dataEnd)
        return new CScriptLex(this, lastPosition, lastCharIdx);
    else
        return new CScriptLex(this, lastPosition, dataEnd );
}

constexpr string CScriptLex::getPosition(int pos) {
    if (pos<0) pos=tokenLastEnd;
    int line = 1,col = 1;
    for (int i=0;i<pos;i++) {
        char ch;
        if (i < dataEnd)
            ch = data[i];
        else
            ch = 0;
        col++;
        if (ch=='\n') {
            line++;
            col = 0;
        }
    }
	// std::string ret = "(line: " + std::to_string(line) + ", col: " + std::to_string(col) + ")";  // TODO:
    // return ret;
	return "(line: 0, col: 0)";
}

// ----------------------------------------------------------------------------------- CSCRIPTVARLINK

constexpr CScriptVarLink::CScriptVarLink(CScriptVar *var, const std::string &name) {
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    this->name = name;
    this->nextSibling = 0;
    this->prevSibling = 0;
    this->var = var->ref();
    this->owned = false;
}

constexpr CScriptVarLink::CScriptVarLink(const CScriptVarLink &link) {
    // Copy constructor
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    this->name = link.name;
    this->nextSibling = 0;
    this->prevSibling = 0;
    this->var = link.var->ref();
    this->owned = false;
}

constexpr CScriptVarLink::~CScriptVarLink() {
#if DEBUG_MEMORY
    mark_deallocated(this);
#endif
    var->unref();
}

constexpr void CScriptVarLink::replaceWith(CScriptVar *newVar) {
    CScriptVar *oldVar = var;
    var = newVar->ref();
    oldVar->unref();
}

constexpr void CScriptVarLink::replaceWith(CScriptVarLink *newVar) {
    if (newVar)
      replaceWith(newVar->var);
    else
      replaceWith(new CScriptVar());
}

constexpr int CScriptVarLink::getIntName() {
    return ::stoi(name);
}

constexpr void CScriptVarLink::setIntName(int n) {
    char sIdx[64];
    sprintf_s(sIdx, sizeof(sIdx), "%d", n);
    name = sIdx;
}

// ----------------------------------------------------------------------------------- CSCRIPTVAR

constexpr CScriptVar::CScriptVar() {
    refs = 0;
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    init();
    flags = SCRIPTVAR_UNDEFINED;
}

constexpr CScriptVar::CScriptVar(const string &str) {
    refs = 0;
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    init();
    flags = SCRIPTVAR_STRING;
    data = str;
}


constexpr CScriptVar::CScriptVar(const string &varData, int varFlags) {
    refs = 0;
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    init();
    flags = varFlags;
    if (varFlags & SCRIPTVAR_INTEGER) {
      intData = ::stol(varData);
    } else if (varFlags & SCRIPTVAR_DOUBLE) {
      doubleData = 0;  // TODO:
    } else
      data = varData;
}

constexpr CScriptVar::CScriptVar(double val) {
    refs = 0;
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    init();
    setDouble(val);
}

constexpr CScriptVar::CScriptVar(int val) {
    refs = 0;
#if DEBUG_MEMORY
    mark_allocated(this);
#endif
    init();
    setInt(val);
}

constexpr CScriptVar::~CScriptVar(void) {
#if DEBUG_MEMORY
    mark_deallocated(this);
#endif
    removeAllChildren();
}

constexpr void CScriptVar::init() {
    firstChild = 0;
    lastChild = 0;
    flags = 0;
    jsCallback = 0;
    jsCallbackUserData = 0;
    data = TINYJS_BLANK_DATA;
    intData = 0;
    doubleData = 0;
}

constexpr CScriptVar *CScriptVar::getReturnVar() {
    return getParameter(TINYJS_RETURN_VAR);
}

constexpr void CScriptVar::setReturnVar(CScriptVar *var) {
    findChildOrCreate(TINYJS_RETURN_VAR)->replaceWith(var);
}


constexpr CScriptVar *CScriptVar::getParameter(const std::string &name) {
    return findChildOrCreate(name)->var;
}

constexpr CScriptVarLink *CScriptVar::findChild(const string &childName) {
    CScriptVarLink *v = firstChild;
    while (v) {
        if (v->name.compare(childName)==0)
            return v;
        v = v->nextSibling;
    }
    return 0;
}

constexpr CScriptVarLink *CScriptVar::findChildOrCreate(const string &childName, int varFlags) {
    CScriptVarLink *l = findChild(childName);
    if (l) return l;

    return addChild(childName, new CScriptVar(TINYJS_BLANK_DATA, varFlags));
}

constexpr CScriptVarLink *CScriptVar::findChildOrCreateByPath(const std::string &path) {
  size_t p = path.find('.');
  if (p == string::npos)
    return findChildOrCreate(path);

  return findChildOrCreate(path.substr(0,p), SCRIPTVAR_OBJECT)->var->
            findChildOrCreateByPath(path.substr(p+1));
}

constexpr CScriptVarLink *CScriptVar::addChild(const std::string &childName, CScriptVar *child) {
  if (isUndefined()) {
    flags = SCRIPTVAR_OBJECT;
  }
    // if no child supplied, create one
    if (!child)
      child = new CScriptVar();

    CScriptVarLink *link = new CScriptVarLink(child, childName);
    link->owned = true;
    if (lastChild) {
        lastChild->nextSibling = link;
        link->prevSibling = lastChild;
        lastChild = link;
    } else {
        firstChild = link;
        lastChild = link;
    }
    return link;
}

constexpr CScriptVarLink *CScriptVar::addChildNoDup(const std::string &childName, CScriptVar *child) {
    // if no child supplied, create one
    if (!child)
      child = new CScriptVar();

    CScriptVarLink *v = findChild(childName);
    if (v) {
        v->replaceWith(child);
    } else {
        v = addChild(childName, child);
    }

    return v;
}

constexpr void CScriptVar::removeChild(CScriptVar *child) {
    CScriptVarLink *link = firstChild;
    while (link) {
        if (link->var == child)
            break;
        link = link->nextSibling;
    }
    ASSERT(link);
    removeLink(link);
}

constexpr void CScriptVar::removeLink(CScriptVarLink *link) {
    if (!link) return;
    if (link->nextSibling)
      link->nextSibling->prevSibling = link->prevSibling;
    if (link->prevSibling)
      link->prevSibling->nextSibling = link->nextSibling;
    if (lastChild == link)
        lastChild = link->prevSibling;
    if (firstChild == link)
        firstChild = link->nextSibling;
    delete link;
}

constexpr void CScriptVar::removeAllChildren() {
    CScriptVarLink *c = firstChild;
    while (c) {
        CScriptVarLink *t = c->nextSibling;
        delete c;
        c = t;
    }
    firstChild = 0;
    lastChild = 0;
}

constexpr CScriptVar *CScriptVar::getArrayIndex(int idx) {
    char sIdx[64];
    sprintf_s(sIdx, sizeof(sIdx), "%d", idx);
    CScriptVarLink *link = findChild(sIdx);
    if (link) return link->var;
    else return new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_NULL); // undefined
}

constexpr void CScriptVar::setArrayIndex(int idx, CScriptVar *value) {
    char sIdx[64];
    sprintf_s(sIdx, sizeof(sIdx), "%d", idx);
    CScriptVarLink *link = findChild(sIdx);

    if (link) {
      if (value->isUndefined())
        removeLink(link);
      else
        link->replaceWith(value);
    } else {
      if (!value->isUndefined())
        addChild(sIdx, value);
    }
}

constexpr int CScriptVar::getArrayLength() {
    int highest = -1;
    if (!isArray()) return 0;

    CScriptVarLink *link = firstChild;
    while (link) {
      if (isNumber(link->name)) {
        int val = atoi(link->name.c_str());
        if (val > highest) highest = val;
      }
      link = link->nextSibling;
    }
    return highest+1;
}

constexpr int CScriptVar::getChildren() {
    int n = 0;
    CScriptVarLink *link = firstChild;
    while (link) {
      n++;
      link = link->nextSibling;
    }
    return n;
}

constexpr int CScriptVar::getInt() {
    /* strtol understands about hex and octal */
    if (isInt()) return intData;
    if (isNull()) return 0;
    if (isUndefined()) return 0;
    if (isDouble()) return (int)doubleData;
    return 0;
}

constexpr double CScriptVar::getDouble() {
    if (isDouble()) return doubleData;
    if (isInt()) return intData;
    if (isNull()) return 0;
    if (isUndefined()) return 0;
    return 0; /* or NaN? */
}

constexpr const string &CScriptVar::getString() {
    /* Because we can't return a string that is generated on demand.
     * I should really just use char* :) */
    if (isInt()) {
      char buffer[32];
      sprintf_s(buffer, sizeof(buffer), "%ld", intData);
      data = buffer;
    }
    if (isDouble()) {
      char buffer[32];
      sprintf_s(buffer, sizeof(buffer), "%f", doubleData);
      data = buffer;
    }
    if (isNull()) data = "null";
    if (isUndefined()) data = "undefined";
    // are we just a string here?
    return data;
}

constexpr void CScriptVar::setInt(int val) {
    flags = (flags&~SCRIPTVAR_VARTYPEMASK) | SCRIPTVAR_INTEGER;
    intData = val;
    doubleData = 0;
    data = TINYJS_BLANK_DATA;
}

constexpr void CScriptVar::setDouble(double val) {
    flags = (flags&~SCRIPTVAR_VARTYPEMASK) | SCRIPTVAR_DOUBLE;
    doubleData = val;
    intData = 0;
    data = TINYJS_BLANK_DATA;
}

constexpr void CScriptVar::setString(const string &str) {
    // name sure it's not still a number or integer
    flags = (flags&~SCRIPTVAR_VARTYPEMASK) | SCRIPTVAR_STRING;
    data = str;
    intData = 0;
    doubleData = 0;
}

constexpr void CScriptVar::setUndefined() {
    // name sure it's not still a number or integer
    flags = (flags&~SCRIPTVAR_VARTYPEMASK) | SCRIPTVAR_UNDEFINED;
    data = TINYJS_BLANK_DATA;
    intData = 0;
    doubleData = 0;
    removeAllChildren();
}

constexpr void CScriptVar::setArray() {
    // name sure it's not still a number or integer
    flags = (flags&~SCRIPTVAR_VARTYPEMASK) | SCRIPTVAR_ARRAY;
    data = TINYJS_BLANK_DATA;
    intData = 0;
    doubleData = 0;
    removeAllChildren();
}

constexpr bool CScriptVar::equals(CScriptVar *v) {
    CScriptVar *resV = mathsOp(v, LEX_EQUAL);
    bool res = resV->getBool();
    delete resV;
    return res;
}

constexpr CScriptVar *CScriptVar::mathsOp(CScriptVar *b, int op) {
    CScriptVar *a = this;
    // Type equality check
    if (op == LEX_TYPEEQUAL || op == LEX_NTYPEEQUAL) {
      // check type first, then call again to check data
      bool eql = ((a->flags & SCRIPTVAR_VARTYPEMASK) ==
                  (b->flags & SCRIPTVAR_VARTYPEMASK));
      if (eql) {
        CScriptVar *contents = a->mathsOp(b, LEX_EQUAL);
        if (!contents->getBool()) eql = false;
        if (!contents->refs) delete contents;
      }
                 ;
      if (op == LEX_TYPEEQUAL)
        return new CScriptVar(eql);
      else
        return new CScriptVar(!eql);
    }
    // do maths...
    if (a->isUndefined() && b->isUndefined()) {
      if (op == LEX_EQUAL) return new CScriptVar(true);
      else if (op == LEX_NEQUAL) return new CScriptVar(false);
      else return new CScriptVar(); // undefined
    } else if ((a->isNumeric() || a->isUndefined()) &&
               (b->isNumeric() || b->isUndefined())) {
        if (!a->isDouble() && !b->isDouble()) {
            // use ints
            int da = a->getInt();
            int db = b->getInt();
            switch (op) {
                case '+': return new CScriptVar(da+db);
                case '-': return new CScriptVar(da-db);
                case '*': return new CScriptVar(da*db);
                case '/': return new CScriptVar(da/db);
                case '&': return new CScriptVar(da&db);
                case '|': return new CScriptVar(da|db);
                case '^': return new CScriptVar(da^db);
                case '%': return new CScriptVar(da%db);
                case LEX_EQUAL:     return new CScriptVar(da==db);
                case LEX_NEQUAL:    return new CScriptVar(da!=db);
                case '<':     return new CScriptVar(da<db);
                case LEX_LEQUAL:    return new CScriptVar(da<=db);
                case '>':     return new CScriptVar(da>db);
                case LEX_GEQUAL:    return new CScriptVar(da>=db);
                default: throw new CScriptException("Operation "+CScriptLex::getTokenStr(op)+" not supported on the Int datatype");
            }
        } else {
            // use doubles
            double da = a->getDouble();
            double db = b->getDouble();
            switch (op) {
                case '+': return new CScriptVar(da+db);
                case '-': return new CScriptVar(da-db);
                case '*': return new CScriptVar(da*db);
                case '/': return new CScriptVar(da/db);
                case LEX_EQUAL:     return new CScriptVar(da==db);
                case LEX_NEQUAL:    return new CScriptVar(da!=db);
                case '<':     return new CScriptVar(da<db);
                case LEX_LEQUAL:    return new CScriptVar(da<=db);
                case '>':     return new CScriptVar(da>db);
                case LEX_GEQUAL:    return new CScriptVar(da>=db);
                default: throw new CScriptException("Operation "+CScriptLex::getTokenStr(op)+" not supported on the Double datatype");
            }
        }
    } else if (a->isArray()) {
      /* Just check pointers */
      switch (op) {
           case LEX_EQUAL: return new CScriptVar(a==b);
           case LEX_NEQUAL: return new CScriptVar(a!=b);
           default: throw new CScriptException("Operation "+CScriptLex::getTokenStr(op)+" not supported on the Array datatype");
      }
    } else if (a->isObject()) {
          /* Just check pointers */
          switch (op) {
               case LEX_EQUAL: return new CScriptVar(a==b);
               case LEX_NEQUAL: return new CScriptVar(a!=b);
               default: throw new CScriptException("Operation "+CScriptLex::getTokenStr(op)+" not supported on the Object datatype");
          }
    } else {
       string da = a->getString();
       string db = b->getString();
       // use strings
       switch (op) {
           case '+':           return new CScriptVar(da+db, SCRIPTVAR_STRING);
           case LEX_EQUAL:     return new CScriptVar(da==db);
           case LEX_NEQUAL:    return new CScriptVar(da!=db);
           case '<':     return new CScriptVar(da<db);
           case LEX_LEQUAL:    return new CScriptVar(da<=db);
           case '>':     return new CScriptVar(da>db);
           case LEX_GEQUAL:    return new CScriptVar(da>=db);
           default: throw new CScriptException("Operation "+CScriptLex::getTokenStr(op)+" not supported on the string datatype");
       }
    }
    ASSERT(0);
    return 0;
}

constexpr void CScriptVar::copySimpleData(CScriptVar *val) {
    data = val->data;
    intData = val->intData;
    doubleData = val->doubleData;
    flags = (flags & ~SCRIPTVAR_VARTYPEMASK) | (val->flags & SCRIPTVAR_VARTYPEMASK);
}

constexpr void CScriptVar::copyValue(CScriptVar *val) {
    if (val) {
      copySimpleData(val);
      // remove all current children
      removeAllChildren();
      // copy children of 'val'
      CScriptVarLink *child = val->firstChild;
      while (child) {
        CScriptVar *copied;
        // don't copy the 'parent' object...
        if (child->name != TINYJS_PROTOTYPE_CLASS)
          copied = child->var->deepCopy();
        else
          copied = child->var;

        addChild(child->name, copied);

        child = child->nextSibling;
      }
    } else {
      setUndefined();
    }
}

constexpr CScriptVar *CScriptVar::deepCopy() {
    CScriptVar *newVar = new CScriptVar();
    newVar->copySimpleData(this);
    // copy children
    CScriptVarLink *child = firstChild;
    while (child) {
        CScriptVar *copied;
        // don't copy the 'parent' object...
        if (child->name != TINYJS_PROTOTYPE_CLASS)
          copied = child->var->deepCopy();
        else
          copied = child->var;

        newVar->addChild(child->name, copied);
        child = child->nextSibling;
    }
    return newVar;
}

constexpr void CScriptVar::trace(string indentStr, const string &name) {
    TRACE("%s'%s' = '%s' %s\n",
        indentStr.c_str(),
        name.c_str(),
        getString().c_str(),
        getFlagsAsString().c_str());
    string indent = indentStr+" ";
    CScriptVarLink *link = firstChild;
    while (link) {
      link->var->trace(indent, link->name);
      link = link->nextSibling;
    }
}

constexpr string CScriptVar::getFlagsAsString() {
  string flagstr = "";
  if (flags&SCRIPTVAR_FUNCTION) flagstr = flagstr + "FUNCTION ";
  if (flags&SCRIPTVAR_OBJECT) flagstr = flagstr + "OBJECT ";
  if (flags&SCRIPTVAR_ARRAY) flagstr = flagstr + "ARRAY ";
  if (flags&SCRIPTVAR_NATIVE) flagstr = flagstr + "NATIVE ";
  if (flags&SCRIPTVAR_DOUBLE) flagstr = flagstr + "DOUBLE ";
  if (flags&SCRIPTVAR_INTEGER) flagstr = flagstr + "INTEGER ";
  if (flags&SCRIPTVAR_STRING) flagstr = flagstr + "STRING ";
  return flagstr;
}

constexpr string CScriptVar::getParsableString() {
  // Numbers can just be put in directly
  if (isNumeric())
    return getString();
  if (isFunction()) {
    ostringstream funcStr;
    funcStr << "function (";
    // get list of parameters
    CScriptVarLink *link = firstChild;
    while (link) {
      funcStr << link->name;
      if (link->nextSibling) funcStr << ",";
      link = link->nextSibling;
    }
    // add function body
    funcStr << ") " << getString();
    return funcStr.str();
  }
  // if it is a string then we quote it
  if (isString())
    return getJSString(getString());
  if (isNull())
      return "null";
  return "undefined";
}

constexpr std::string CScriptVar::getJSON(const string linePrefix) {
   std::string res;
   if (isObject()) {
      string indentedLinePrefix = linePrefix+"  ";
      // children - handle with bracketed list
      res += "{ \n";
      CScriptVarLink *link = firstChild;
      while (link) {
        res += indentedLinePrefix;
        res += getJSString(link->name);
        res += " : ";
        res += link->var->getJSON(indentedLinePrefix);
        link = link->nextSibling;
        if (link) {
          res += ",\n";
        }
      }
      res += "\n" + linePrefix + "}";
    } else if (isArray()) {
      string indentedLinePrefix = linePrefix+"  ";
      res += "[\n";
      int len = getArrayLength();
      if (len>10000) len=10000; // we don't want to get stuck here!

      for (int i=0;i<len;i++) {
        res += getArrayIndex(i)->getJSON(indentedLinePrefix);
        if (i<len-1) res += ",\n";
      }

      res += "\n" + linePrefix + "]";
    } else {
      // no children or a function... just write value directly
      res += getParsableString();
    }
	return res;
}


constexpr void CScriptVar::setCallback(JSCallback callback, void *userdata) {
    jsCallback = callback;
    jsCallbackUserData = userdata;
}

constexpr CScriptVar *CScriptVar::ref() {
    refs++;
    return this;
}

constexpr void CScriptVar::unref() {
    if (refs<=0) printf("OMFG, we have unreffed too far!\n");
    if ((--refs)==0) {
      delete this;
    }
}

constexpr int CScriptVar::getRefs() {
    return refs;
}
// ----------------------------------------------------------------------------------- CSCRIPT

constexpr CTinyJS::CTinyJS()
    : root{(new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT))->ref()}
    , stringClass{(new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT))->ref()}
    , objectClass{(new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT))->ref()}
    , arrayClass{(new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT))->ref()}
{
    root->addChild("String", stringClass);
    root->addChild("Array", arrayClass);
    root->addChild("Object", objectClass);
}

constexpr CTinyJS::~CTinyJS() {
    ASSERT(!l);
    scopes.clear();
    stringClass->unref();
    arrayClass->unref();
    objectClass->unref();
    root->unref();

#if DEBUG_MEMORY
    show_allocated();
#endif
}

constexpr void CTinyJS::trace() {
    root->trace();
}

constexpr void CTinyJS::execute(const string &code) {
    CScriptLex *oldLex = l;
    vector<CScriptVar*> oldScopes = scopes;
    l = new CScriptLex(code);
#ifdef TINYJS_CALL_STACK
    call_stack.clear();
#endif
    scopes.clear();
    scopes.push_back(root);
    try {
        bool execute = true;
        while (l->tk) statement(execute);
    } catch (CScriptException *e) {
        ostringstream msg;
        msg << "Error " << e->text;
#ifdef TINYJS_CALL_STACK
        for (int i=(int)call_stack.size()-1;i>=0;i--)
          msg << "\n" << i << ": " << call_stack.at(i);
#endif
        msg << " at " << l->getPosition();
        delete l;
        l = oldLex;

        throw new CScriptException(msg.str());
    }
    delete l;
    l = oldLex;
    scopes = oldScopes;
}

constexpr CScriptVarLink CTinyJS::evaluateComplex(const string &code) {
    CScriptLex *oldLex = l;
    vector<CScriptVar*> oldScopes = scopes;

    l = new CScriptLex(code);
#ifdef TINYJS_CALL_STACK
    call_stack.clear();
#endif
    scopes.clear();
    scopes.push_back(root);
    CScriptVarLink *v = 0;
    try {
        bool execute = true;
        do {
          CLEAN(v);
          v = base(execute);
          if (l->tk!=LEX_EOF) l->match(';');
        } while (l->tk!=LEX_EOF);
    } catch (CScriptException *e) {
      ostringstream msg;
      msg << "Error " << e->text;
#ifdef TINYJS_CALL_STACK
      for (int i=(int)call_stack.size()-1;i>=0;i--)
        msg << "\n" << i << ": " << call_stack.at(i);
#endif
      msg << " at " << l->getPosition();
      delete l;
      l = oldLex;

        throw new CScriptException(msg.str());
    }
    delete l;
    l = oldLex;
    scopes = oldScopes;

    if (v) {
        CScriptVarLink r = *v;
        CLEAN(v);
        return r;
    }
    // return undefined...
    return CScriptVarLink(new CScriptVar());
}

constexpr string CTinyJS::evaluate(const string &code) {
    return evaluateComplex(code).var->getString();
}

constexpr void CTinyJS::parseFunctionArguments(CScriptVar *funcVar) {
  l->match('(');
  while (l->tk!=')') {
      funcVar->addChildNoDup(l->tkStr);
      l->match(LEX_ID);
      if (l->tk!=')') l->match(',');
  }
  l->match(')');
}

constexpr void CTinyJS::addNative(const string &funcDesc, JSCallback ptr, void *userdata) {
    CScriptLex *oldLex = l;
    l = new CScriptLex(funcDesc);

    CScriptVar *base = root;

    l->match(LEX_R_FUNCTION);
    string funcName = l->tkStr;
    l->match(LEX_ID);
    /* Check for dots, we might want to do something like function String.substring ... */
    while (l->tk == '.') {
      l->match('.');
      CScriptVarLink *link = base->findChild(funcName);
      // if it doesn't exist, make an object class
      if (!link) link = base->addChild(funcName, new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT));
      base = link->var;
      funcName = l->tkStr;
      l->match(LEX_ID);
    }

    CScriptVar *funcVar = new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_FUNCTION | SCRIPTVAR_NATIVE);
    funcVar->setCallback(ptr, userdata);
    parseFunctionArguments(funcVar);
    delete l;
    l = oldLex;

    base->addChild(funcName, funcVar);
}

constexpr CScriptVarLink *CTinyJS::parseFunctionDefinition() {
  // actually parse a function...
  l->match(LEX_R_FUNCTION);
  string funcName = TINYJS_TEMP_NAME;
  /* we can have functions without names */
  if (l->tk==LEX_ID) {
    funcName = l->tkStr;
    l->match(LEX_ID);
  }
  CScriptVarLink *funcVar = new CScriptVarLink(new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_FUNCTION), funcName);
  parseFunctionArguments(funcVar->var);
  int funcBegin = l->tokenStart;
  bool noexecute = false;
  block(noexecute);
  funcVar->var->data = l->getSubString(funcBegin);
  return funcVar;
}

/** Handle a function call (assumes we've parsed the function name and we're
 * on the start bracket). 'parent' is the object that contains this method,
 * if there was one (otherwise it's just a normnal function).
 */
constexpr CScriptVarLink *CTinyJS::functionCall(bool &execute, CScriptVarLink *function, CScriptVar *parent) {
  if (execute) {
    if (!function->var->isFunction()) {
        string errorMsg = "Expecting '";
        errorMsg = errorMsg + function->name + "' to be a function";
        throw new CScriptException(errorMsg.c_str());
    }
    l->match('(');
    // create a new symbol table entry for execution of this function
    CScriptVar *functionRoot = new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_FUNCTION);
    if (parent)
      functionRoot->addChildNoDup("this", parent);
    // grab in all parameters
    CScriptVarLink *v = function->var->firstChild;
    while (v) {
        CScriptVarLink *value = base(execute);
        if (execute) {
            if (value->var->isBasic()) {
              // pass by value
              functionRoot->addChild(v->name, value->var->deepCopy());
            } else {
              // pass by reference
              functionRoot->addChild(v->name, value->var);
            }
        }
        CLEAN(value);
        if (l->tk!=')') l->match(',');
        v = v->nextSibling;
    }
    l->match(')');
    // setup a return variable
    CScriptVarLink *returnVar = NULL;
    // execute function!
    // add the function's execute space to the symbol table so we can recurse
    CScriptVarLink *returnVarLink = functionRoot->addChild(TINYJS_RETURN_VAR);
    scopes.push_back(functionRoot);
#ifdef TINYJS_CALL_STACK
    call_stack.push_back(function->name + " from " + l->getPosition());
#endif

    if (function->var->isNative()) {
        ASSERT(function->var->jsCallback);
        function->var->jsCallback(functionRoot, function->var->jsCallbackUserData);
    } else {
        /* we just want to execute the block, but something could
         * have messed up and left us with the wrong ScriptLex, so
         * we want to be careful here... */
        CScriptException *exception = 0;
        CScriptLex *oldLex = l;
        CScriptLex *newLex = new CScriptLex(function->var->getString());
        l = newLex;
        try {
          block(execute);
          // because return will probably have called this, and set execute to false
          execute = true;
        } catch (CScriptException *e) {
          exception = e;
        }
        delete newLex;
        l = oldLex;

        if (exception)
          throw exception;
    }
#ifdef TINYJS_CALL_STACK
    if (!call_stack.empty()) call_stack.pop_back();
#endif
    scopes.pop_back();
    /* get the real return var before we remove it from our function */
    returnVar = new CScriptVarLink(returnVarLink->var);
    functionRoot->removeLink(returnVarLink);
    delete functionRoot;
    if (returnVar)
      return returnVar;
    else
      return new CScriptVarLink(new CScriptVar());
  } else {
    // function, but not executing - just parse args and be done
    l->match('(');
    while (l->tk != ')') {
      CScriptVarLink *value = base(execute);
      CLEAN(value);
      if (l->tk!=')') l->match(',');
    }
    l->match(')');
    if (l->tk == '{') { // TODO: why is this here?
      block(execute);
    }
    /* function will be a blank scriptvarlink if we're not executing,
     * so just return it rather than an alloc/free */
    return function;
  }
}

constexpr CScriptVarLink *CTinyJS::factor(bool &execute) {
    if (l->tk=='(') {
        l->match('(');
        CScriptVarLink *a = base(execute);
        l->match(')');
        return a;
    }
    if (l->tk==LEX_R_TRUE) {
        l->match(LEX_R_TRUE);
        return new CScriptVarLink(new CScriptVar(1));
    }
    if (l->tk==LEX_R_FALSE) {
        l->match(LEX_R_FALSE);
        return new CScriptVarLink(new CScriptVar(0));
    }
    if (l->tk==LEX_R_NULL) {
        l->match(LEX_R_NULL);
        return new CScriptVarLink(new CScriptVar(TINYJS_BLANK_DATA,SCRIPTVAR_NULL));
    }
    if (l->tk==LEX_R_UNDEFINED) {
        l->match(LEX_R_UNDEFINED);
        return new CScriptVarLink(new CScriptVar(TINYJS_BLANK_DATA,SCRIPTVAR_UNDEFINED));
    }
    if (l->tk==LEX_ID) {
        CScriptVarLink *a = execute ? findInScopes(l->tkStr) : new CScriptVarLink(new CScriptVar());
        //printf("0x%08X for %s at %s\n", (unsigned int)a, l->tkStr.c_str(), l->getPosition().c_str());
        /* The parent if we're executing a method call */
        CScriptVar *parent = 0;

        if (execute && !a) {
          /* Variable doesn't exist! JavaScript says we should create it
           * (we won't add it here. This is done in the assignment operator)*/
          a = new CScriptVarLink(new CScriptVar(), l->tkStr);
        }
        l->match(LEX_ID);
        while (l->tk=='(' || l->tk=='.' || l->tk=='[') {
            if (l->tk=='(') { // ------------------------------------- Function Call
                a = functionCall(execute, a, parent);
            } else if (l->tk == '.') { // ------------------------------------- Record Access
                l->match('.');
                if (execute) {
                  const string &name = l->tkStr;
                  CScriptVarLink *child = a->var->findChild(name);
                  if (!child) child = findInParentClasses(a->var, name);
                  if (!child) {
                    /* if we haven't found this defined yet, use the built-in
                       'length' properly */
                    if (a->var->isArray() && name == "length") {
                      int l = a->var->getArrayLength();
                      child = new CScriptVarLink(new CScriptVar(l));
                    } else if (a->var->isString() && name == "length") {
                      int l = a->var->getString().size();
                      child = new CScriptVarLink(new CScriptVar(l));
                    } else {
                      child = a->var->addChild(name);
                    }
                  }
                  parent = a->var;
                  a = child;
                }
                l->match(LEX_ID);
            } else if (l->tk == '[') { // ------------------------------------- Array Access
                l->match('[');
                CScriptVarLink *index = base(execute);
                l->match(']');
                if (execute) {
                  CScriptVarLink *child = a->var->findChildOrCreate(index->var->getString());
                  parent = a->var;
                  a = child;
                }
                CLEAN(index);
            } else ASSERT(0);
        }
        return a;
    }
    if (l->tk==LEX_INT || l->tk==LEX_FLOAT) {
        CScriptVar *a = new CScriptVar(l->tkStr,
            ((l->tk==LEX_INT)?SCRIPTVAR_INTEGER:SCRIPTVAR_DOUBLE));
        l->match(l->tk);
        return new CScriptVarLink(a);
    }
    if (l->tk==LEX_STR) {
        CScriptVar *a = new CScriptVar(l->tkStr, SCRIPTVAR_STRING);
        l->match(LEX_STR);
        return new CScriptVarLink(a);
    }
    if (l->tk=='{') {
        CScriptVar *contents = new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT);
        /* JSON-style object definition */
        l->match('{');
        while (l->tk != '}') {
          string id = l->tkStr;
          // we only allow strings or IDs on the left hand side of an initialisation
          if (l->tk==LEX_STR) l->match(LEX_STR);
          else l->match(LEX_ID);
          l->match(':');
          if (execute) {
            CScriptVarLink *a = base(execute);
            contents->addChild(id, a->var);
            CLEAN(a);
          }
          // no need to clean here, as it will definitely be used
          if (l->tk != '}') l->match(',');
        }

        l->match('}');
        return new CScriptVarLink(contents);
    }
    if (l->tk=='[') {
        CScriptVar *contents = new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_ARRAY);
        /* JSON-style array */
        l->match('[');
        int idx = 0;
        while (l->tk != ']') {
          if (execute) {
            char idx_str[16]; // big enough for 2^32
            sprintf_s(idx_str, sizeof(idx_str), "%d",idx);

            CScriptVarLink *a = base(execute);
            contents->addChild(idx_str, a->var);
            CLEAN(a);
          }
          // no need to clean here, as it will definitely be used
          if (l->tk != ']') l->match(',');
          idx++;
        }
        l->match(']');
        return new CScriptVarLink(contents);
    }
    if (l->tk==LEX_R_FUNCTION) {
      CScriptVarLink *funcVar = parseFunctionDefinition();
        if (funcVar->name != TINYJS_TEMP_NAME)
          TRACE("Functions not defined at statement-level are not meant to have a name");
        return funcVar;
    }
    if (l->tk==LEX_R_NEW) {
      // new -> create a new object
      l->match(LEX_R_NEW);
      const string &className = l->tkStr;
      if (execute) {
        CScriptVarLink *objClassOrFunc = findInScopes(className);
        if (!objClassOrFunc) {
          TRACE("%s is not a valid class name", className.c_str());
          return new CScriptVarLink(new CScriptVar());
        }
        l->match(LEX_ID);
        CScriptVar *obj = new CScriptVar(TINYJS_BLANK_DATA, SCRIPTVAR_OBJECT);
        CScriptVarLink *objLink = new CScriptVarLink(obj);
        if (objClassOrFunc->var->isFunction()) {
          CLEAN(functionCall(execute, objClassOrFunc, obj));
        } else {
          obj->addChild(TINYJS_PROTOTYPE_CLASS, objClassOrFunc->var);
          if (l->tk == '(') {
            l->match('(');
            l->match(')');
          }
        }
        return objLink;
      } else {
        l->match(LEX_ID);
        if (l->tk == '(') {
          l->match('(');
          l->match(')');
        }
      }
    }
    // Nothing we can do here... just hope it's the end...
    l->match(LEX_EOF);
    return 0;
}

constexpr CScriptVarLink *CTinyJS::unary(bool &execute) {
    CScriptVarLink *a;
    if (l->tk=='!') {
        l->match('!'); // binary not
        a = factor(execute);
        if (execute) {
            CScriptVar zero(0);
            CScriptVar *res = a->var->mathsOp(&zero, LEX_EQUAL);
            CREATE_LINK(a, res);
        }
    } else
        a = factor(execute);
    return a;
}

constexpr CScriptVarLink *CTinyJS::term(bool &execute) {
    CScriptVarLink *a = unary(execute);
    while (l->tk=='*' || l->tk=='/' || l->tk=='%') {
        int op = l->tk;
        l->match(l->tk);
        CScriptVarLink *b = unary(execute);
        if (execute) {
            CScriptVar *res = a->var->mathsOp(b->var, op);
            CREATE_LINK(a, res);
        }
        CLEAN(b);
    }
    return a;
}

constexpr CScriptVarLink *CTinyJS::expression(bool &execute) {
    bool negate = false;
    if (l->tk=='-') {
        l->match('-');
        negate = true;
    }
    CScriptVarLink *a = term(execute);
    if (negate) {
        CScriptVar zero(0);
        CScriptVar *res = zero.mathsOp(a->var, '-');
        CREATE_LINK(a, res);
    }

    while (l->tk=='+' || l->tk=='-' ||
        l->tk==LEX_PLUSPLUS || l->tk==LEX_MINUSMINUS) {
        int op = l->tk;
        l->match(l->tk);
        if (op==LEX_PLUSPLUS || op==LEX_MINUSMINUS) {
            if (execute) {
                CScriptVar one(1);
                CScriptVar *res = a->var->mathsOp(&one, op==LEX_PLUSPLUS ? '+' : '-');
                CScriptVarLink *oldValue = new CScriptVarLink(a->var);
                // in-place add/subtract
                a->replaceWith(res);
                CLEAN(a);
                a = oldValue;
            }
        } else {
            CScriptVarLink *b = term(execute);
            if (execute) {
                // not in-place, so just replace
                CScriptVar *res = a->var->mathsOp(b->var, op);
                CREATE_LINK(a, res);
            }
            CLEAN(b);
        }
    }
    return a;
}

constexpr CScriptVarLink *CTinyJS::shift(bool &execute) {
  CScriptVarLink *a = expression(execute);
  if (l->tk==LEX_LSHIFT || l->tk==LEX_RSHIFT || l->tk==LEX_RSHIFTUNSIGNED) {
    int op = l->tk;
    l->match(op);
    CScriptVarLink *b = base(execute);
    int shift = execute ? b->var->getInt() : 0;
    CLEAN(b);
    if (execute) {
      if (op==LEX_LSHIFT) a->var->setInt(a->var->getInt() << shift);
      if (op==LEX_RSHIFT) a->var->setInt(a->var->getInt() >> shift);
      if (op==LEX_RSHIFTUNSIGNED) a->var->setInt(((unsigned int)a->var->getInt()) >> shift);
    }
  }
  return a;
}

constexpr CScriptVarLink *CTinyJS::condition(bool &execute) {
    CScriptVarLink *a = shift(execute);
    CScriptVarLink *b;
    while (l->tk==LEX_EQUAL || l->tk==LEX_NEQUAL ||
           l->tk==LEX_TYPEEQUAL || l->tk==LEX_NTYPEEQUAL ||
           l->tk==LEX_LEQUAL || l->tk==LEX_GEQUAL ||
           l->tk=='<' || l->tk=='>') {
        int op = l->tk;
        l->match(l->tk);
        b = shift(execute);
        if (execute) {
            CScriptVar *res = a->var->mathsOp(b->var, op);
            CREATE_LINK(a,res);
        }
        CLEAN(b);
    }
    return a;
}

constexpr CScriptVarLink *CTinyJS::logic(bool &execute) {
    CScriptVarLink *a = condition(execute);
    CScriptVarLink *b;
    while (l->tk=='&' || l->tk=='|' || l->tk=='^' || l->tk==LEX_ANDAND || l->tk==LEX_OROR) {
        bool noexecute = false;
        int op = l->tk;
        l->match(l->tk);
        bool shortCircuit = false;
        bool boolean = false;
        // if we have short-circuit ops, then if we know the outcome
        // we don't bother to execute the other op. Even if not
        // we need to tell mathsOp it's an & or |
        if (op==LEX_ANDAND) {
            op = '&';
            shortCircuit = !a->var->getBool();
            boolean = true;
        } else if (op==LEX_OROR) {
            op = '|';
            shortCircuit = a->var->getBool();
            boolean = true;
        }
        b = condition(shortCircuit ? noexecute : execute);
        if (execute && !shortCircuit) {
            if (boolean) {
              CScriptVar *newa = new CScriptVar(a->var->getBool());
              CScriptVar *newb = new CScriptVar(b->var->getBool());
              CREATE_LINK(a, newa);
              CREATE_LINK(b, newb);
            }
            CScriptVar *res = a->var->mathsOp(b->var, op);
            CREATE_LINK(a, res);
        }
        CLEAN(b);
    }
    return a;
}

constexpr CScriptVarLink *CTinyJS::ternary(bool &execute) {
  CScriptVarLink *lhs = logic(execute);
  bool noexec = false;
  if (l->tk=='?') {
    l->match('?');
    if (!execute) {
      CLEAN(lhs);
      CLEAN(base(noexec));
      l->match(':');
      CLEAN(base(noexec));
    } else {
      bool first = lhs->var->getBool();
      CLEAN(lhs);
      if (first) {
        lhs = base(execute);
        l->match(':');
        CLEAN(base(noexec));
      } else {
        CLEAN(base(noexec));
        l->match(':');
        lhs = base(execute);
      }
    }
  }

  return lhs;
}

constexpr CScriptVarLink *CTinyJS::base(bool &execute) {
    CScriptVarLink *lhs = ternary(execute);
    if (l->tk=='=' || l->tk==LEX_PLUSEQUAL || l->tk==LEX_MINUSEQUAL) {
        /* If we're assigning to this and we don't have a parent,
         * add it to the symbol table root as per JavaScript. */
        if (execute && !lhs->owned) {
          if (lhs->name.length()>0) {
            CScriptVarLink *realLhs = root->addChildNoDup(lhs->name, lhs->var);
            CLEAN(lhs);
            lhs = realLhs;
          } else
            TRACE("Trying to assign to an un-named type\n");
        }

        int op = l->tk;
        l->match(l->tk);
        CScriptVarLink *rhs = base(execute);
        if (execute) {
            if (op=='=') {
                lhs->replaceWith(rhs);
            } else if (op==LEX_PLUSEQUAL) {
                CScriptVar *res = lhs->var->mathsOp(rhs->var, '+');
                lhs->replaceWith(res);
            } else if (op==LEX_MINUSEQUAL) {
                CScriptVar *res = lhs->var->mathsOp(rhs->var, '-');
                lhs->replaceWith(res);
            } else ASSERT(0);
        }
        CLEAN(rhs);
    }
    return lhs;
}

constexpr void CTinyJS::block(bool &execute) {
    l->match('{');
    if (execute) {
      while (l->tk && l->tk!='}')
        statement(execute);
      l->match('}');
    } else {
      // fast skip of blocks
      int brackets = 1;
      while (l->tk && brackets) {
        if (l->tk == '{') brackets++;
        if (l->tk == '}') brackets--;
        l->match(l->tk);
      }
    }

}

constexpr void CTinyJS::statement(bool &execute) {
    if (l->tk==LEX_ID ||
        l->tk==LEX_INT ||
        l->tk==LEX_FLOAT ||
        l->tk==LEX_STR ||
        l->tk=='-') {
        /* Execute a simple statement that only contains basic arithmetic... */
        CLEAN(base(execute));
        l->match(';');
    } else if (l->tk=='{') {
        /* A block of code */
        block(execute);
    } else if (l->tk==';') {
        /* Empty statement - to allow things like ;;; */
        l->match(';');
    } else if (l->tk==LEX_R_VAR) {
        /* variable creation. TODO - we need a better way of parsing the left
         * hand side. Maybe just have a flag called can_create_var that we
         * set and then we parse as if we're doing a normal equals.*/
        l->match(LEX_R_VAR);
        while (l->tk != ';') {
          CScriptVarLink *a = 0;
          if (execute)
            a = scopes.back()->findChildOrCreate(l->tkStr);
          l->match(LEX_ID);
          // now do stuff defined with dots
          while (l->tk == '.') {
              l->match('.');
              if (execute) {
                  CScriptVarLink *lastA = a;
                  a = lastA->var->findChildOrCreate(l->tkStr);
              }
              l->match(LEX_ID);
          }
          // sort out initialiser
          if (l->tk == '=') {
              l->match('=');
              CScriptVarLink *var = base(execute);
              if (execute)
                  a->replaceWith(var);
              CLEAN(var);
          }
          if (l->tk != ';')
            l->match(',');
        }
        l->match(';');
    } else if (l->tk==LEX_R_IF) {
        l->match(LEX_R_IF);
        l->match('(');
        CScriptVarLink *var = base(execute);
        l->match(')');
        bool cond = execute && var->var->getBool();
        CLEAN(var);
        bool noexecute = false; // because we need to be abl;e to write to it
        statement(cond ? execute : noexecute);
        if (l->tk==LEX_R_ELSE) {
            l->match(LEX_R_ELSE);
            statement(cond ? noexecute : execute);
        }
    } else if (l->tk==LEX_R_WHILE) {
        // We do repetition by pulling out the string representing our statement
        // there's definitely some opportunity for optimisation here
        l->match(LEX_R_WHILE);
        l->match('(');
        int whileCondStart = l->tokenStart;
        bool noexecute = false;
        CScriptVarLink *cond = base(execute);
        bool loopCond = execute && cond->var->getBool();
        CLEAN(cond);
        CScriptLex *whileCond = l->getSubLex(whileCondStart);
        l->match(')');
        int whileBodyStart = l->tokenStart;
        statement(loopCond ? execute : noexecute);
        CScriptLex *whileBody = l->getSubLex(whileBodyStart);
        CScriptLex *oldLex = l;
        int loopCount = TINYJS_LOOP_MAX_ITERATIONS;
        while (loopCond && loopCount-->0) {
            whileCond->reset();
            l = whileCond;
            cond = base(execute);
            loopCond = execute && cond->var->getBool();
            CLEAN(cond);
            if (loopCond) {
                whileBody->reset();
                l = whileBody;
                statement(execute);
            }
        }
        l = oldLex;
        delete whileCond;
        delete whileBody;

        if (loopCount<=0) {
            root->trace();
            TRACE("WHILE Loop exceeded %d iterations at %s\n", TINYJS_LOOP_MAX_ITERATIONS, l->getPosition().c_str());
            throw new CScriptException("LOOP_ERROR");
        }
    } else if (l->tk==LEX_R_FOR) {
        l->match(LEX_R_FOR);
        l->match('(');
        statement(execute); // initialisation
        //l->match(';');
        int forCondStart = l->tokenStart;
        bool noexecute = false;
        CScriptVarLink *cond = base(execute); // condition
        bool loopCond = execute && cond->var->getBool();
        CLEAN(cond);
        CScriptLex *forCond = l->getSubLex(forCondStart);
        l->match(';');
        int forIterStart = l->tokenStart;
        CLEAN(base(noexecute)); // iterator
        CScriptLex *forIter = l->getSubLex(forIterStart);
        l->match(')');
        int forBodyStart = l->tokenStart;
        statement(loopCond ? execute : noexecute);
        CScriptLex *forBody = l->getSubLex(forBodyStart);
        CScriptLex *oldLex = l;
        if (loopCond) {
            forIter->reset();
            l = forIter;
            CLEAN(base(execute));
        }
        int loopCount = TINYJS_LOOP_MAX_ITERATIONS;
        while (execute && loopCond && loopCount-->0) {
            forCond->reset();
            l = forCond;
            cond = base(execute);
            loopCond = cond->var->getBool();
            CLEAN(cond);
            if (execute && loopCond) {
                forBody->reset();
                l = forBody;
                statement(execute);
            }
            if (execute && loopCond) {
                forIter->reset();
                l = forIter;
                CLEAN(base(execute));
            }
        }
        l = oldLex;
        delete forCond;
        delete forIter;
        delete forBody;
        if (loopCount<=0) {
            root->trace();
            TRACE("FOR Loop exceeded %d iterations at %s\n", TINYJS_LOOP_MAX_ITERATIONS, l->getPosition().c_str());
            throw new CScriptException("LOOP_ERROR");
        }
    } else if (l->tk==LEX_R_RETURN) {
        l->match(LEX_R_RETURN);
        CScriptVarLink *result = 0;
        if (l->tk != ';')
          result = base(execute);
        if (execute) {
          CScriptVarLink *resultVar = scopes.back()->findChild(TINYJS_RETURN_VAR);
          if (resultVar)
            resultVar->replaceWith(result);
          else
            TRACE("RETURN statement, but not in a function.\n");
          execute = false;
        }
        CLEAN(result);
        l->match(';');
    } else if (l->tk==LEX_R_FUNCTION) {
        CScriptVarLink *funcVar = parseFunctionDefinition();
        if (execute) {
          if (funcVar->name == TINYJS_TEMP_NAME)
            TRACE("Functions defined at statement-level are meant to have a name\n");
          else
            scopes.back()->addChildNoDup(funcVar->name, funcVar->var);
        }
        CLEAN(funcVar);
    } else l->match(LEX_EOF);
}

/// Get the given variable specified by a path (var1.var2.etc), or return 0
constexpr CScriptVar *CTinyJS::getScriptVariable(const string &path) {
    // traverse path
    size_t prevIdx = 0;
    size_t thisIdx = path.find('.');
    if (thisIdx == string::npos) thisIdx = path.length();
    CScriptVar *var = root;
    while (var && prevIdx<path.length()) {
        string el = path.substr(prevIdx, thisIdx-prevIdx);
        CScriptVarLink *varl = var->findChild(el);
        var = varl?varl->var:0;
        prevIdx = thisIdx+1;
        thisIdx = path.find('.', prevIdx);
        if (thisIdx == string::npos) thisIdx = path.length();
    }
    return var;
}

/// Get the value of the given variable, or return 0
constexpr const string *CTinyJS::getVariable(const string &path) {
    CScriptVar *var = getScriptVariable(path);
    // return result
    if (var)
        return &var->getString();
    else
        return 0;
}

/// set the value of the given variable, return trur if it exists and gets set
constexpr bool CTinyJS::setVariable(const std::string &path, const std::string &varData) {
    CScriptVar *var = getScriptVariable(path);
    // return result
    if (var) {
        if (var->isInt())
            var->setInt((int)strtol(varData.c_str(),0,0));
        else if (var->isDouble())
            var->setDouble(strtod(varData.c_str(),0));
        else
            var->setString(varData.c_str());
        return true;
    }
    else
        return false;
}

/// Finds a child, looking recursively up the scopes
constexpr CScriptVarLink *CTinyJS::findInScopes(const std::string &childName) {
    for (int s=scopes.size()-1;s>=0;s--) {
      CScriptVarLink *v = scopes[s]->findChild(childName);
      if (v) return v;
    }
    return NULL;

}

/// Look up in any parent classes of the given object
constexpr CScriptVarLink *CTinyJS::findInParentClasses(CScriptVar *object, const std::string &name) {
    // Look for links to actual parent classes
    CScriptVarLink *parentClass = object->findChild(TINYJS_PROTOTYPE_CLASS);
    while (parentClass) {
      CScriptVarLink *implementation = parentClass->var->findChild(name);
      if (implementation) return implementation;
      parentClass = parentClass->var->findChild(TINYJS_PROTOTYPE_CLASS);
    }
    // else fake it for strings and finally objects
    if (object->isString()) {
      CScriptVarLink *implementation = stringClass->findChild(name);
      if (implementation) return implementation;
    }
    if (object->isArray()) {
      CScriptVarLink *implementation = arrayClass->findChild(name);
      if (implementation) return implementation;
    }
    CScriptVarLink *implementation = objectClass->findChild(name);
    if (implementation) return implementation;

    return 0;
}

#endif  // TINYJS_HPP
