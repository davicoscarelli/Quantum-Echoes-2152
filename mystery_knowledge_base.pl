%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deduction Engine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Dynamic declarations for updating during the game
:- dynamic lab_access/2.
:- dynamic background/2.
:- dynamic evidence/3.

% Deduces potential suspects and asks for more detailed evidence checking
find_suspects(Suspect, Request) :-
    background(Suspect, espionage),
    lab_access(Suspect, false),
    gather_relevant_evidence(Suspect, EvidenceList),
    required_evidence_types(RequiredTypes),
    (   has_required_evidence(EvidenceList, RequiredTypes) ->
        Request = 'No further evidence needed';
        Request = 'Please provide more evidence for this suspect'
    ).


% Advanced rule to deduce the mastermind based on specific types of evidence
deduce_mastermind(Suspect, Details) :-
    find_suspects(Suspect, Request),
    gather_relevant_evidence(Suspect, EvidenceList),
    required_evidence_types(RequiredTypes),
    has_required_evidence(EvidenceList, RequiredTypes),
    Details = [suspect: Suspect, evidence: EvidenceList, status: 'Confirmed Mastermind'].

% Helper to gather all relevant evidence for a suspect
gather_relevant_evidence(Suspect, EvidenceList) :-
    findall(Type-Detail, 
            (   evidence(Suspect, Type, Detail) ;
                evidence(vostok_lab, Type, Detail) ;  % Including evidence related to vostok_lab
                evidence(vostok, Type, Detail)        % Including evidence related to vostoks actions
            ), EvidenceList).
    
% Check if all required evidence types are present
has_required_evidence(EvidenceList, RequiredTypes) :-
    forall(member(Type, RequiredTypes), member(Type-_, EvidenceList)).

% Define required evidence types for confirming a mastermind
required_evidence_types([threat, bribe, footage, ethical_violation]).

% Debugging to show gathered evidence more appropriately
show_evidence(Suspect) :-
    gather_relevant_evidence(Suspect, EvidenceList),
    format('Gathered Relevant Evidence for ~w: ~w~n', [Suspect, EvidenceList]).

% Debugging to check if requirements are met specifically for a suspect
check_requirements(Suspect) :-
    required_evidence_types(RequiredTypes),
    gather_relevant_evidence(Suspect, EvidenceList),
    (   has_required_evidence(EvidenceList, RequiredTypes) ->
        format('All required evidence types are present for ~w.~n', [Suspect]);
        format('Missing required evidence types for ~w.~n', [Suspect])
    ).

% More detailed listing of facts for debugging
list_facts :-
    write('Lab Access Facts:'), nl,
    forall(lab_access(Person, Status), format('~w had lab access: ~w~n', [Person, Status])),
    write('Background Facts:'), nl,
    forall(background(Person, Type), format('~w has background in: ~w~n', [Person, Type])),
    write('Evidence Facts:'), nl,
    forall(evidence(Person, Type, Detail), format('Evidence on ~w about: ~w, Details: ~w~n', [Person, Type, Detail])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DataSleuth
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic person/3.
:- dynamic article/3.

% People profiles
person('Dr. Alice Smith', 'Tech Innovations Lab', ['AI', 'neural networks']).
person('Dr. Bob Jones', 'Quantum Computing Inc', ['quantum computing']).

% Articles
article('Quantum Computing Breakthrough', 'A major advancement in quantum computing has been achieved by Dr. Bob Jones.', ['quantum computing', 'breakthrough']).
article('AI Ethics Debate', 'The recent debate on AI ethics has sparked controversy across the tech world.', ['AI', 'ethics']).

% Search by tag
search(Tag, Result) :-
    person(Name, Affiliation, Tags),
    member(Tag, Tags),
    Result = person(Name, Affiliation, Tags);
    article(Title, Content, Tags),
    member(Tag, Tags),
    Result = article(Title, Content).
