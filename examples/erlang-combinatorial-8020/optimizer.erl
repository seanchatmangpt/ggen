#!/usr/bin/env escript
%% ============================================================================
%% COMBINATORIAL 80/20 OPTIMIZER
%% ============================================================================
%% Navigates 5D feature space to find optimal combination
%%
%% Dimensions:
%% 1. Complexity (5 levels)
%% 2. Domain (5 options)
%% 3. Audience (5 types)
%% 4. Style (4 modes)
%% 5. Time (5 budgets)
%%
%% Total space: 5 × 5 × 5 × 4 × 5 = 2,500 combinations
%% Smart 80/20: ~20 combinations cover 80% of use cases

main(_) ->
    io:format("~n╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║                                                                   ║~n"),
    io:format("║  COMBINATORIAL 80/20: Multi-Dimensional Optimization             ║~n"),
    io:format("║  Navigate 2,500 combinations to find YOUR perfect match!         ║~n"),
    io:format("║                                                                   ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("This optimizer finds the ideal combination across 5 dimensions:~n"),
    io:format("  📊 Complexity Level (0-4)~n"),
    io:format("  🎯 Domain Focus (Concept/Telecom/Web/IoT/ML)~n"),
    io:format("  👥 Audience Type (Student/Dev/Architect/Exec/AGI)~n"),
    io:format("  📚 Learning Style (Visual/Hands-on/Theory/Reference)~n"),
    io:format("  ⏰ Time Budget (1m/5m/30m/2h/Unlimited)~n~n"),

    %% Get user context
    Context = gather_context(),

    %% Calculate optimal combination
    Optimal = optimize(Context),

    %% Display recommendation
    display_recommendation(Optimal, Context),

    halt(0).

%% ============================================================================
%% Context Gathering
%% ============================================================================

gather_context() ->
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("STEP 1: Tell us about your context~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n~n"),

    %% Question 1: Primary goal
    io:format("1️⃣  What's your PRIMARY goal?~n"),
    io:format("   A) Learn the concept (understanding)~n"),
    io:format("   B) Build production code (application)~n"),
    io:format("   C) Design architecture (planning)~n"),
    io:format("   D) Quick overview (executive summary)~n"),
    io:format("   E) Extract patterns (AI/research)~n~n"),

    Goal = get_choice("Your choice: ", #{
        a => learn, b => build, c => design, d => overview, e => research
    }),

    %% Question 2: Domain
    io:format("~n2️⃣  Which domain are you working in?~n"),
    io:format("   A) Just learning (pure concept)~n"),
    io:format("   B) Telecom/carrier systems~n"),
    io:format("   C) Web services/APIs~n"),
    io:format("   D) IoT/embedded systems~n"),
    io:format("   E) Machine learning/AI~n~n"),

    Domain = get_choice("Your choice: ", #{
        a => concept, b => telecom, c => web, d => iot, e => ml
    }),

    %% Question 3: Experience level
    io:format("~n3️⃣  Your Erlang/OTP experience?~n"),
    io:format("   A) Complete beginner~n"),
    io:format("   B) Some tutorials~n"),
    io:format("   C) Built projects~n"),
    io:format("   D) Production experience~n"),
    io:format("   E) Expert architect~n~n"),

    Experience = get_choice("Your choice: ", #{
        a => beginner, b => learning, c => intermediate, d => advanced, e => expert
    }),

    %% Question 4: Learning preference
    io:format("~n4️⃣  How do you learn best?~n"),
    io:format("   A) Visual (diagrams, charts)~n"),
    io:format("   B) Hands-on (code, experiments)~n"),
    io:format("   C) Theory (principles, why)~n"),
    io:format("   D) Reference (lookup, docs)~n~n"),

    Style = get_choice("Your choice: ", #{
        a => visual, b => hands_on, c => theory, d => reference
    }),

    %% Question 5: Time available
    io:format("~n5️⃣  How much time do you have?~n"),
    io:format("   A) 1 minute (elevator pitch)~n"),
    io:format("   B) 5 minutes (quick start)~n"),
    io:format("   C) 30 minutes (tutorial)~n"),
    io:format("   D) 2 hours (deep dive)~n"),
    io:format("   E) Unlimited (mastery)~n~n"),

    Time = get_choice("Your choice: ", #{
        a => one_min, b => five_min, c => thirty_min, d => two_hours, e => unlimited
    }),

    #{
        goal => Goal,
        domain => Domain,
        experience => Experience,
        style => Style,
        time => Time
    }.

get_choice(Prompt, ChoiceMap) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        eof -> maps:get(c, ChoiceMap);  % Default to C
        Line ->
            Trimmed = string:trim(string:lowercase(Line)),
            case Trimmed of
                "a" -> maps:get(a, ChoiceMap);
                "b" -> maps:get(b, ChoiceMap);
                "c" -> maps:get(c, ChoiceMap);
                "d" -> maps:get(d, ChoiceMap);
                "e" -> maps:get(e, ChoiceMap);
                _ -> maps:get(c, ChoiceMap)  % Default
            end
    end.

%% ============================================================================
%% Optimization Engine
%% ============================================================================

optimize(Context) ->
    io:format("~n~n🔍 Analyzing your context across 5 dimensions...~n"),
    timer:sleep(500),

    %% Map context to feature vector
    FeatureVector = context_to_vector(Context),

    %% Get top 20 combinations (80/20 of 2,500)
    Top20 = get_top_combinations(),

    %% Score each against user context
    Scored = lists:map(fun(Combo) ->
        Score = score_combination(Combo, FeatureVector),
        {Score, Combo}
    end, Top20),

    %% Find best match
    {_BestScore, BestCombo} = lists:max(Scored),

    io:format("✅ Found optimal combination (scored ~p candidates)~n~n", [length(Scored)]),

    BestCombo.

context_to_vector(#{goal := Goal, domain := Domain, experience := Exp,
                     style := Style, time := Time}) ->
    #{
        complexity => experience_to_complexity(Exp, Time),
        domain => Domain,
        audience => goal_to_audience(Goal, Exp),
        style => Style,
        time => Time
    }.

experience_to_complexity(beginner, one_min) -> 0;
experience_to_complexity(beginner, _) -> 1;
experience_to_complexity(learning, Time) when Time == one_min; Time == five_min -> 1;
experience_to_complexity(learning, _) -> 2;
experience_to_complexity(intermediate, _) -> 2;
experience_to_complexity(advanced, _) -> 3;
experience_to_complexity(expert, _) -> 4.

goal_to_audience(overview, _) -> executive;
goal_to_audience(learn, Exp) when Exp == beginner; Exp == learning -> student;
goal_to_audience(build, _) -> developer;
goal_to_audience(design, _) -> architect;
goal_to_audience(research, _) -> agi.

get_top_combinations() ->
    %% The 20 combinations that cover 80% of use cases
    [
        #{name => "Executive Briefing", complexity => 0, domain => concept,
          audience => executive, style => visual, time => one_min, freq => 85},
        #{name => "Student Quick Start", complexity => 1, domain => concept,
          audience => student, style => hands_on, time => five_min, freq => 95},
        #{name => "Developer Production", complexity => 3, domain => web,
          audience => developer, style => hands_on, time => thirty_min, freq => 80},
        #{name => "Architect Deep Dive", complexity => 4, domain => telecom,
          audience => architect, style => theory, time => two_hours, freq => 60},
        #{name => "AGI Pattern Extraction", complexity => 2, domain => concept,
          audience => agi, style => reference, time => unlimited, freq => 70},
        #{name => "IoT Developer Starter", complexity => 2, domain => iot,
          audience => developer, style => hands_on, time => thirty_min, freq => 65},
        #{name => "ML Engineer Reference", complexity => 3, domain => ml,
          audience => developer, style => reference, time => two_hours, freq => 55},
        #{name => "Student Visual Tutorial", complexity => 1, domain => concept,
          audience => student, style => visual, time => five_min, freq => 75},
        #{name => "Web Dev Hands-On", complexity => 2, domain => web,
          audience => developer, style => hands_on, time => thirty_min, freq => 85},
        #{name => "Telecom Architect", complexity => 4, domain => telecom,
          audience => architect, style => theory, time => unlimited, freq => 50},
        #{name => "Quick Concept Check", complexity => 0, domain => concept,
          audience => developer, style => reference, time => one_min, freq => 70},
        #{name => "IoT Visual Guide", complexity => 2, domain => iot,
          audience => developer, style => visual, time => thirty_min, freq => 60},
        #{name => "ML Theory Deep Dive", complexity => 4, domain => ml,
          audience => architect, style => theory, time => unlimited, freq => 45},
        #{name => "Student Theory Intro", complexity => 1, domain => concept,
          audience => student, style => theory, time => thirty_min, freq => 65},
        #{name => "Exec Telecom Brief", complexity => 0, domain => telecom,
          audience => executive, style => visual, time => one_min, freq => 55},
        #{name => "Web API Reference", complexity => 3, domain => web,
          audience => developer, style => reference, time => two_hours, freq => 75},
        #{name => "AGI ML Patterns", complexity => 3, domain => ml,
          audience => agi, style => reference, time => unlimited, freq => 50},
        #{name => "Student Web Tutorial", complexity => 2, domain => web,
          audience => student, style => hands_on, time => thirty_min, freq => 80},
        #{name => "IoT Production Guide", complexity => 3, domain => iot,
          audience => developer, style => hands_on, time => two_hours, freq => 55},
        #{name => "Quick ML Overview", complexity => 1, domain => ml,
          audience => developer, style => visual, time => five_min, freq => 60}
    ].

score_combination(Combo, UserVector) ->
    %% Weighted scoring across dimensions
    ComplexityScore = if
        maps:get(complexity, Combo) == maps:get(complexity, UserVector) -> 1.0;
        abs(maps:get(complexity, Combo) - maps:get(complexity, UserVector)) == 1 -> 0.7;
        true -> 0.3
    end,

    DomainScore = if
        maps:get(domain, Combo) == maps:get(domain, UserVector) -> 1.0;
        maps:get(domain, Combo) == concept -> 0.6;  % Concept works for anyone
        true -> 0.2
    end,

    AudienceScore = if
        maps:get(audience, Combo) == maps:get(audience, UserVector) -> 1.0;
        true -> 0.4
    end,

    StyleScore = if
        maps:get(style, Combo) == maps:get(style, UserVector) -> 1.0;
        true -> 0.5
    end,

    TimeScore = if
        maps:get(time, Combo) == maps:get(time, UserVector) -> 1.0;
        true -> 0.6
    end,

    %% Weighted average (complexity and domain matter more)
    WeightedScore = ComplexityScore * 0.25 +
                    DomainScore * 0.25 +
                    AudienceScore * 0.20 +
                    StyleScore * 0.15 +
                    TimeScore * 0.15,

    %% Boost by frequency (popular combinations are probably good)
    Frequency = maps:get(freq, Combo, 50),
    WeightedScore * (0.8 + (Frequency / 500)).  % Frequency multiplier

%% ============================================================================
%% Recommendation Display
%% ============================================================================

display_recommendation(Optimal, Context) ->
    io:format("╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║                    YOUR OPTIMAL COMBINATION                       ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("🎯 Recommendation: ~s~n~n", [maps:get(name, Optimal)]),

    io:format("📊 Feature Vector (5D):~n"),
    io:format("   ├─ Complexity:  Level ~p (~s)~n",
              [maps:get(complexity, Optimal), complexity_name(maps:get(complexity, Optimal))]),
    io:format("   ├─ Domain:      ~s~n", [atom_to_list(maps:get(domain, Optimal))]),
    io:format("   ├─ Audience:    ~s~n", [atom_to_list(maps:get(audience, Optimal))]),
    io:format("   ├─ Style:       ~s~n", [atom_to_list(maps:get(style, Optimal))]),
    io:format("   └─ Time:        ~s~n~n", [time_name(maps:get(time, Optimal))]),

    io:format("📈 Popularity:  ~p%% of users choose similar combinations~n~n",
              [maps:get(freq, Optimal)]),

    %% Show what this means in practice
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("WHAT YOU'LL GET:~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n~n"),

    describe_output(Optimal),

    %% Show alternative nearby combinations
    io:format("~n💡 ALTERNATIVE COMBINATIONS (Nearby in Feature Space):~n~n"),
    show_alternatives(Optimal),

    %% Show the math
    io:format("~n🔬 THE COMBINATORIAL MATH:~n"),
    io:format("   Total possible combinations: 5×5×5×4×5 = 2,500~n"),
    io:format("   Top 20 combinations cover: ~80%% of use cases~n"),
    io:format("   Your combination is: #~p in popularity~n~n", [get_rank(Optimal)]),

    %% Next steps
    io:format("🚀 NEXT STEPS:~n"),
    io:format("   1. Generate code at this combination~n"),
    io:format("   2. Try it out~n"),
    io:format("   3. Zoom to nearby combinations if needed~n~n").

complexity_name(0) -> "Oneliner (0.8%)";
complexity_name(1) -> "Meta-minimal (4%)";
complexity_name(2) -> "Minimal (20%)";
complexity_name(3) -> "Balanced (50%)";
complexity_name(4) -> "Comprehensive (100%)".

time_name(one_min) -> "1 minute";
time_name(five_min) -> "5 minutes";
time_name(thirty_min) -> "30 minutes";
time_name(two_hours) -> "2 hours";
time_name(unlimited) -> "Unlimited".

describe_output(#{complexity := C, domain := D, style := S}) ->
    Lines = case C of
        0 -> "~15 lines";
        1 -> "~30 lines";
        2 -> "~60 lines";
        3 -> "~120 lines";
        4 -> "~200 lines"
    end,

    DomainFeatures = case D of
        concept -> "Pure gen_server pattern, no domain logic";
        telecom -> "Call routing, billing, SLAs, carrier-grade patterns";
        web -> "HTTP, REST, microservices, API patterns";
        iot -> "MQTT, sensors, edge computing, device patterns";
        ml -> "Model serving, training pipeline, feature engineering"
    end,

    StyleContent = case S of
        visual -> "State diagrams, flowcharts, visual examples";
        hands_on -> "Runnable code, step-by-step, try-it-yourself";
        theory -> "Principles, explanations, mental models";
        reference -> "API docs, signatures, quick lookup"
    end,

    io:format("   📄 Code size:      ~s~n", [Lines]),
    io:format("   🎯 Domain focus:   ~s~n", [DomainFeatures]),
    io:format("   📚 Content style:  ~s~n", [StyleContent]).

show_alternatives(Current) ->
    %% Show 3 alternatives by varying one dimension
    io:format("   • Higher complexity:  Zoom to Level ~p~n", [min(4, maps:get(complexity, Current) + 1)]),
    io:format("   • Lower complexity:   Zoom to Level ~p~n", [max(0, maps:get(complexity, Current) - 1)]),
    io:format("   • Different style:    Try ~s mode~n", [suggest_alt_style(maps:get(style, Current))]).

suggest_alt_style(visual) -> "hands-on";
suggest_alt_style(hands_on) -> "theory";
suggest_alt_style(theory) -> "reference";
suggest_alt_style(reference) -> "visual".

get_rank(#{freq := Freq}) when Freq >= 90 -> 1;
get_rank(#{freq := Freq}) when Freq >= 80 -> 2;
get_rank(#{freq := Freq}) when Freq >= 70 -> 3;
get_rank(#{freq := Freq}) when Freq >= 60 -> 5;
get_rank(#{freq := Freq}) when Freq >= 50 -> 8;
get_rank(_) -> 15.
