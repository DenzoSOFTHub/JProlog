% DCG Test 05: String Validation Patterns
% Tests: Email validation, phone numbers, identifiers

% Character classes
letter --> [C], { C >= 97, C =< 122 }.  % lowercase a-z
letter --> [C], { C >= 65, C =< 90 }.   % uppercase A-Z
digit --> [C], { C >= 48, C =< 57 }.    % digits 0-9
underscore --> [95].                    % '_'
at --> [64].                            % '@'
dot --> [46].                           % '.'
hyphen --> [45].                        % '-'

% Identifier (starts with letter, then letters/digits/underscore)
identifier --> letter, id_rest.
id_rest --> [].
id_rest --> letter, id_rest.
id_rest --> digit, id_rest.
id_rest --> underscore, id_rest.

% Domain name component
domain_part --> letter, domain_rest.
domain_rest --> [].
domain_rest --> letter, domain_rest.
domain_rest --> digit, domain_rest.
domain_rest --> hyphen, domain_rest.

% Simple email validation
email --> identifier, at, domain_part, dot, domain_part.

% Phone number (simple format: digits, hyphens allowed)
phone_digit --> digit.
phone_digit --> hyphen.

phone --> phone_digits.
phone_digits --> phone_digit.
phone_digits --> phone_digit, phone_digits.

% URL validation (simplified)
http --> [104,116,116,112].  % "http"
https --> [104,116,116,112,115].  % "https"
colon_slash_slash --> [58,47,47].  % "://"

url --> http, colon_slash_slash, domain_part.
url --> https, colon_slash_slash, domain_part.

% Test queries:
% ?- phrase(identifier, [104,101,108,108,111]).     % Expected: true ("hello")
% ?- phrase(email, [116,101,115,116,64,101,120,97,109,112,108,101,46,99,111,109]). % Expected: true
% ?- phrase(phone, [49,50,51,45,52,53,54]).         % Expected: true ("123-456")