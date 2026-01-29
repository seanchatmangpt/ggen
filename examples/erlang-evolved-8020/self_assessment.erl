#!/usr/bin/env escript
%% ============================================================================
%% EVOLVED 80/20: Self-Assessment Tool
%% ============================================================================
%% This tool helps you find the optimal complexity level based on:
%% - Your learning goals
%% - Time budget
%% - Current expertise
%% - Preferred learning style
%%
%% Innovation: Quantifiable complexity matching instead of guessing

main(_) ->
    io:format("~n╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║                                                                   ║~n"),
    io:format("║  EVOLVED 80/20: Find Your Optimal Complexity Level               ║~n"),
    io:format("║                                                                   ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("This 5-question assessment will recommend the perfect complexity~n"),
    io:format("level for your learning journey.~n~n"),

    %% Question 1: Time budget
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Question 1: How much time do you have?~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("  A) 1 minute  - Just show me the absolute essence~n"),
    io:format("  B) 3 minutes - Quick critical path only~n"),
    io:format("  C) 5 minutes - Full understanding of core concept~n"),
    io:format("  D) 30+ minutes - Production-ready implementation~n"),
    io:format("  E) Unlimited - Complete mastery with all features~n~n"),

    TimeScore = get_answer("Your choice (A/B/C/D/E): ",
                           #{a => 0, b => 1, c => 2, d => 3, e => 4}),

    %% Question 2: Learning goal
    io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Question 2: What's your primary goal?~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("  A) Understand the concept (learning)~n"),
    io:format("  B) Prototype quickly (speed)~n"),
    io:format("  C) Build production system (quality)~n"),
    io:format("  D) Teach others (pedagogy)~n"),
    io:format("  E) Architecture review (depth)~n~n"),

    GoalScore = get_answer("Your choice (A/B/C/D/E): ",
                          #{a => 1, b => 0, c => 4, d => 2, e => 3}),

    %% Question 3: Erlang experience
    io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Question 3: Your Erlang/OTP experience?~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("  A) Never used Erlang~n"),
    io:format("  B) Read tutorials, no practice~n"),
    io:format("  C) Built toy projects~n"),
    io:format("  D) Production experience~n"),
    io:format("  E) Expert (5+ years)~n~n"),

    ExpScore = get_answer("Your choice (A/B/C/D/E): ",
                         #{a => 0, b => 1, c => 2, d => 3, e => 4}),

    %% Question 4: Learning style
    io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Question 4: How do you prefer to learn?~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("  A) Show me one example, I'll figure out the rest~n"),
    io:format("  B) Quick overview, then I explore~n"),
    io:format("  C) Balanced explanation with examples~n"),
    io:format("  D) Detailed walkthrough with edge cases~n"),
    io:format("  E) Complete reference with all details~n~n"),

    StyleScore = get_answer("Your choice (A/B/C/D/E): ",
                           #{a => 0, b => 1, c => 2, d => 3, e => 4}),

    %% Question 5: Complexity preference
    io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Question 5: When facing something new, you prefer...~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("  A) Simplest possible version~n"),
    io:format("  B) Just enough to understand~n"),
    io:format("  C) Core features, no extras~n"),
    io:format("  D) Most common use cases~n"),
    io:format("  E) Everything available~n~n"),

    PrefScore = get_answer("Your choice (A/B/C/D/E): ",
                          #{a => 0, b => 1, c => 2, d => 3, e => 4}),

    %% Calculate recommendation
    TotalScore = TimeScore + GoalScore + ExpScore + StyleScore + PrefScore,
    AvgScore = TotalScore / 5.0,

    Level = if
        AvgScore < 0.5 -> 0;
        AvgScore < 1.5 -> 1;
        AvgScore < 2.5 -> 2;
        AvgScore < 3.5 -> 3;
        true -> 4
    end,

    %% Display recommendation
    io:format("~n~n╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║                    RECOMMENDATION                                 ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("Your optimal complexity level: ~p (~s)~n~n", [Level, level_name(Level)]),

    case Level of
        0 ->
            io:format("📌 LEVEL 0: ONELINER (0.8%% of comprehensive)~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Perfect for: Speed learners, concept grasp~n"),
            io:format("Time: 1 minute~n"),
            io:format("Features: Just increment/0 - the absolute essence~n"),
            io:format("Lines of code: ~8 lines~n~n"),
            io:format("Next step: Run `./generate.sh 0` to create Level 0 code~n");

        1 ->
            io:format("📌 LEVEL 1: META-MINIMAL (4%% of comprehensive)~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Perfect for: Rapid learners, critical path focus~n"),
            io:format("Time: 3 minutes~n"),
            io:format("Features: increment/0 + get/0 - read/write basics~n"),
            io:format("Lines of code: ~15 lines~n~n"),
            io:format("Next step: Run `./generate.sh 1` to create Level 1 code~n");

        2 ->
            io:format("📌 LEVEL 2: MINIMAL (20%% of comprehensive)~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Perfect for: Thorough learners, full understanding~n"),
            io:format("Time: 5 minutes~n"),
            io:format("Features: increment/get/reset/add - core operations~n"),
            io:format("Lines of code: ~30 lines~n~n"),
            io:format("Next step: Run `./generate.sh 2` to create Level 2 code~n");

        3 ->
            io:format("📌 LEVEL 3: BALANCED (50%% of comprehensive)~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Perfect for: Production engineers, quality focus~n"),
            io:format("Time: 30 minutes~n"),
            io:format("Features: All operations + metrics + monitoring~n"),
            io:format("Lines of code: ~75 lines~n~n"),
            io:format("Next step: Run `./generate.sh 3` to create Level 3 code~n");

        4 ->
            io:format("📌 LEVEL 4: COMPREHENSIVE (100%%)~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Perfect for: Architects, complete mastery~n"),
            io:format("Time: Unlimited~n"),
            io:format("Features: Everything + pub/sub + advanced patterns~n"),
            io:format("Lines of code: ~120 lines~n~n"),
            io:format("Next step: Run `./generate.sh 4` to create Level 4 code~n")
    end,

    io:format("~n💡 PROGRESSIVE LEARNING PATH:~n"),
    io:format("   Start at your level, then 'zoom in' when ready:~n"),
    io:format("   Level 0 (1 min) → Level 1 (3 min) → Level 2 (5 min) → ...~n~n"),

    io:format("📊 Your Score Breakdown:~n"),
    io:format("   Time budget:    ~.1f~n", [TimeScore]),
    io:format("   Learning goal:  ~.1f~n", [GoalScore]),
    io:format("   Experience:     ~.1f~n", [ExpScore]),
    io:format("   Learning style: ~.1f~n", [StyleScore]),
    io:format("   Complexity pref:~.1f~n", [PrefScore]),
    io:format("   ───────────────────~n"),
    io:format("   Average:        ~.2f  →  Level ~p~n~n", [AvgScore, Level]),

    halt(0).

get_answer(Prompt, ScoreMap) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        eof -> 0.0;
        Line ->
            Trimmed = string:trim(string:lowercase(Line)),
            case Trimmed of
                "a" -> maps:get(a, ScoreMap, 0.0);
                "b" -> maps:get(b, ScoreMap, 0.0);
                "c" -> maps:get(c, ScoreMap, 0.0);
                "d" -> maps:get(d, ScoreMap, 0.0);
                "e" -> maps:get(e, ScoreMap, 0.0);
                _ ->
                    io:format("Invalid choice. Using 'C' as default.~n"),
                    maps:get(c, ScoreMap, 0.0)
            end
    end.

level_name(0) -> "Oneliner - Absolute Essence";
level_name(1) -> "Meta-Minimal - Critical Path";
level_name(2) -> "Minimal - Core Understanding";
level_name(3) -> "Balanced - Production Basics";
level_name(4) -> "Comprehensive - Full Features".
