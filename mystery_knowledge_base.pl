%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Quantum Echoes 2152
% Davi Coscarelli
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deduction Engine App
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Dynamic declarations
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


% Deduce the mastermind based on specific types of evidence
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
                evidence(vostok_lab, Type, Detail) ;  % Evidence related to vostok_lab
                evidence(vostok, Type, Detail)        % Evidence related to vostoks actions
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
% DataSleuth App (search engine)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dynamic declarations
:- dynamic person/7.
:- dynamic article/4.

% People profiles
person('Dr. Vostok', 'Vostok Lab', ['vostok', 'neural networks', 'vostok_lab'], '2102-04-25', 'Moscow', 'vostok@vostok_lab.com', 'PrevTech Co.', 'vostok.png', '').
person('Alex Hargrave', 'Vostok Lab', ['alex', 'quantum computing', 'vostok_lab'], '1225-08-15', 'San Francisco', 'alex@vostok_lab.com', 'Pinnacle Tech', 'alex.png', 'Known for corporate espionage                -Detected Inference Type: espionage-').
person('Dr. Lydia Grant', 'Vostok Lab', ['lydia', 'robotics', 'vostok_lab'], '2098-06-12', 'New York', 'lydia@futuretech.com', 'CyberDynamics', 'lydia.png', '').
person('John', 'Pinnacle Tech', ['john', 'cybersecurity', 'networking'], '2092-07-03', 'London', 'john@pinnacle.com', 'NetSecure', 'john.png', '').

% Articles
article('Quantum Computing Breakthrough', 'A major advancement in quantum computing has been achieved by Dr. Bob Jones.', ['quantum computing', 'breakthrough'], '').
article('AI Ethics Debate', 'The recent debate on AI ethics has sparked controversy across the tech world.', ['ai', 'ethics'], '').
article('The Future of Robotics: An Insight', 'Dr. Lydia Grant discusses potential breakthroughs in robotic autonomy and AI.', ['robotics', 'ai', 'autonomy'], '').
article('Neurotech Revolution: Bridging Minds and Machines', 'Ms. Emily Stone explores the ethical implications of direct brain-computer interfaces.', ['neurotech', 'ethics', 'bci'], '').
article('AI Methods by Dr. Vostok', 'Concerns have been raised about the unorthodox methods employed by Dr. Vostok in her research stirring both intrigue and criticism.', ['ai', 'controversy', 'vostok'], '--Detected Inference Type: ethical_violation--').

% Search by tag
search(Tag, Result) :-
    person(Name, Affiliation, Tags, DOB, CityOfBirth, Email, PreviousEmployer, Picture, Alert),
    member(Tag, Tags),
    Result = person(Name, Affiliation, DOB, CityOfBirth, Email, PreviousEmployer, Picture, Alert);
    article(Title, Content, Tags, Alert),
    member(Tag, Tags),
    Result = article(Title, Content, Alert).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminal App
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dynamic declarations
:- dynamic email_history/2.
:- dynamic decrypt_file/2.
:- dynamic access_log/2.
:- dynamic security_log/2.
:- dynamic help/2.

% Email History
email_history('vostok@vostok_lab.com', '--From: 24v06nyd5hn--You must immediately cease your research or we will take drastic measures.').
email_history('alex@vostok_lab.com', '---From: john@pinnacle.com---\nAlex, your initiative has been noted. An additional $50,000 has been deposited into your account. Ensure the matter with Dr. Vostok is resolved swiftly by April 17th.\n--Detected Inference Type: bribe--').

% Decrypted email sender identity
decrypt('24v06nyd5hn', 'Email sender: Alex - alex@vostok_lab.com\n\n--Detected Inference Type: threat--').

% Access log
access_log('vostok_lab', 'Access log entry: 04/08/2152 23:47 - Unauthorized access by Alex detected. Security protocols breached at external lab door. --Detected Inference Location: vostok_lab; access = false--').


% Security footage
security_log('04/17/2152', 'footage.mp4').

% Help command output
help(all, 'Available Commands:\nemail_history(Email) - Email history.\ndecrypt(encrypted_text) - Decrypts text.\naccess_log(Location) - Access logs.\nsecurity_log(Date) - Provides security footage.').

% Handle commands from the terminal
handle_command(help(all), Result) :- help(all, Result).
handle_command(email_history(Email), Result) :- email_history(Email, Result).
handle_command(decrypt(FileID), Result) :- decrypt(FileID, Result).
handle_command(access_log(Location), Result) :- access_log(Location, Result).
handle_command(security_log(Date), Result) :- security_log(Date, Result).
