package org.antlr.v4.server;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.dfa.DFAState;

import java.util.BitSet;
import java.util.List;

class CollectLexOrParseSyntaxErrors extends BaseErrorListener {
    final JsonArray msgs = new JsonArray();

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                            int line, int charPositionInLine,
                            String msg,
                            org.antlr.v4.runtime.RecognitionException e) {
        final JsonObject err = new JsonObject();
        if ( recognizer instanceof Lexer ) {
            int erridx = ((Lexer) recognizer)._input.index(); // where we detected error
            int startidx = erridx;
            if ( e instanceof LexerNoViableAltException ) {
                startidx = ((LexerNoViableAltException)e).getStartIndex();
            }
            err.addProperty("startidx", startidx);
            err.addProperty("erridx", erridx);
            err.addProperty("line", line);
            err.addProperty("pos", charPositionInLine);
            err.addProperty("msg", msg);
        }
        else {
            Token startToken;
            Token stopToken;
            if ( e instanceof NoViableAltException ) {
                startToken = ((NoViableAltException) e).getStartToken();
                stopToken = e.getOffendingToken();
            }
            else if ( e==null ) {
                startToken = stopToken = (Token)offendingSymbol;
            }
            else {
                startToken = stopToken = e.getOffendingToken();
            }
            err.addProperty("startidx", startToken.getTokenIndex());
            err.addProperty("stopidx", stopToken.getTokenIndex());
            err.addProperty("line", line);
            err.addProperty("pos", charPositionInLine);
            err.addProperty("msg", msg);
        }
        msgs.add(err);
    }

    @Override
	public void reportAmbiguity(Parser recognizer,
								DFA dfa,
								int startIndex,
								int stopIndex,
								boolean exact,
								BitSet ambigAlts,
								ATNConfigSet configs)
	{
        final JsonObject err = new JsonObject();
        String msg = "reportAmbiguity | rulename: " + recognizer.getRuleNames()[dfa.atnStartState.ruleIndex];
        List<DFAState> states = dfa.getStates();
        JsonArray array = new JsonArray();
        for(DFAState s : states)
        {
            array.add(s.toString());
        }
        err.add("dfaStates", array);
        err.addProperty("startidx", startIndex);
        err.addProperty("stopidx", stopIndex);
        err.addProperty("erridx", stopIndex);
        err.addProperty("line", 1);
        err.addProperty("pos", 1);
        err.addProperty("msg", msg);
        
        msgs.add(err);
	}

	@Override
	public void reportAttemptingFullContext(Parser recognizer,
											DFA dfa,
											int startIndex,
											int stopIndex,
											BitSet conflictingAlts,
											ATNConfigSet configs)
	{
        final JsonObject err = new JsonObject();
        String msg = "reportAttemptingFullContext | rulename: " + recognizer.getRuleNames()[dfa.atnStartState.ruleIndex];
        List<DFAState> states = dfa.getStates();
        JsonArray array = new JsonArray();
        for(DFAState s : states)
        {
            array.add(s.toString());
        }
        err.add("dfaStates", array);
        err.addProperty("startidx", startIndex);
        err.addProperty("stopidx", stopIndex);
        err.addProperty("erridx", stopIndex);
        err.addProperty("line", 1);
        err.addProperty("pos", 1);
        err.addProperty("msg", msg);
        
        msgs.add(err);
	}

	@Override
	public void reportContextSensitivity(Parser recognizer,
										 DFA dfa,
										 int startIndex,
										 int stopIndex,
										 int prediction,
										 ATNConfigSet configs)
	{
        final JsonObject err = new JsonObject();
        String msg = "reportContextSensitivity | rulename: " + recognizer.getRuleNames()[dfa.atnStartState.ruleIndex];
        List<DFAState> states = dfa.getStates();
        JsonArray array = new JsonArray();
        for(DFAState s : states)
        {
            array.add(s.toString());
        }
        err.add("dfaStates", array);
        err.addProperty("startidx", startIndex);
        err.addProperty("stopidx", stopIndex);
        err.addProperty("erridx", stopIndex);
        err.addProperty("line", 1);
        err.addProperty("pos", 1);
        err.addProperty("msg", msg);
        
        msgs.add(err);
	}
}
