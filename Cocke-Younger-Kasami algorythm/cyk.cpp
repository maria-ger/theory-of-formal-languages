#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <set>
#include <vector>
#include <map>

struct rule {
    std::string left;
    std::vector <std::vector<std::string>> rights;
};

std::vector<std::string> str_to_vec(std::string& s) {
    std::vector<std::string> vec;
    for (auto& symbol : s) {
        std::string str;
        str.push_back(symbol);
        vec.push_back(str);
    }
    return vec;
}

std::vector<std::vector<std::string>> insert(std::vector<std::vector<std::string>>& v, std::vector<std::string>& elem) {
    for (auto& cur : v) {
        if (cur == elem) return v;
    }
    v.push_back(elem);
    return v;
}

std::vector<std::vector<std::string>> erase_elem(std::vector<std::vector<std::string>>& v, std::vector<std::string> elem) {
    std::vector<std::vector<std::string>> res;
    for (auto it = v.begin(); it != v.end(); ++it) {
        if (*it != elem) {
            res.push_back(*it);
        }
    }
    return res;
}

std::vector<struct rule*> delete_epsilon(std::vector<struct rule*> &grammar) {
    std::set<std::string> eps_nonterm, new_set;
    std::vector<struct rule*> new_grammar;
    for (auto& r : grammar) {
        for (auto& right : r->rights) { 
            if (*right.begin() == "_") {
                eps_nonterm.insert(r->left);
                r->rights = erase_elem(r->rights, right);
                break;
            }
        }
        if (!r->rights.empty()) new_grammar.push_back(r);
    }
    grammar = new_grammar;
    new_set = eps_nonterm;
    while (!new_set.empty()) {
        new_set = std::set<std::string>();
        for (auto& r : grammar) {
            if (eps_nonterm.find(r->left) == eps_nonterm.end()) {
                for (auto& right : r->rights) {
                    std::set<std::string> nonterm;
                    bool success = true;
                    for (auto& symbol : right) {
                        if ((symbol[0] < 'A') || (symbol[0] > 'Z')) {
                            success = false;
                            break;
                        }
                        else if (eps_nonterm.find(symbol) == eps_nonterm.end()) {
                            success = false;
                            break;
                        }
                        else nonterm.insert(r->left);
                    }
                    if (success) {
                        for (auto& elem : nonterm) {
                            new_set.insert(elem);
                            eps_nonterm.insert(elem);
                        }
                        break;
                    }
                    
                }
            }
        }
    }
    for (auto& r : grammar) {
        std::vector <std::vector<std::string>> new_rights;
        for (auto& right_part : r->rights) {
            std::set <std::vector<std::string>> result, prev, added;
            result.insert(right_part);
            new_rights.push_back(right_part);
            prev = result;
            while (true) {
                added = std::set<std::vector<std::string>>();
                for (auto& elem : prev) {
                    if (elem.size() > 1) {
                        for (auto& n : eps_nonterm) {
                            std::vector<std::string> modified;
                            for (auto& c : elem) {
                                if (c != n) {
                                    modified.push_back(c);
                                }
                            }
                            if (!modified.empty() && (modified != elem)) {
                                added.insert(modified);
                                result.insert(modified);
                                new_rights = insert(new_rights, modified);
                            }
                        }
                    }
                }
                if (added.empty()) break;
                prev = added;
            }
        }
        r->rights = new_rights;
    }
    return grammar;
}

std::vector<struct rule*> delete_chains(std::vector<struct rule*>& grammar) {
    std::set<std::vector<std::string>> chain_pairs, added, to_erase;
    for (auto& r : grammar) {
        std::vector<std::string> pair;
        pair.push_back(r->left);
        r->rights = erase_elem(r->rights, pair);
        pair.push_back(r->left);
        chain_pairs.insert(pair);
    }
    added = chain_pairs;
    while (!added.empty()) {
        added = std::set<std::vector<std::string>> ();
        for (auto& r : grammar) {
            for (auto &pair : chain_pairs) {
                if (pair[1] == r->left) {
                    for (auto& right_part : r->rights) {
                        std::string symbol = right_part[0];
                        if ((right_part.size() == 1) && (symbol[0] >= 'A') && (symbol[0] <= 'Z')) {
                            std::vector<std::string> new_pair;
                            new_pair.push_back(pair[0]);
                            new_pair.push_back(symbol);
                            if (chain_pairs.find(new_pair) == chain_pairs.end()) added.insert(new_pair);
                        }
                    }
                }
            }
            for (auto& pair : added) chain_pairs.insert(pair);
        }
    }
    for (auto& pair : chain_pairs) {
        for (auto& r : grammar) {
            if (r->left == pair[0]) {
                std::set<std::vector<std::string>> to_erase;
                for (auto& right_part : r->rights) {
                    if (right_part.size() == 1 && right_part[0] == pair[1]) to_erase.insert(right_part);
                }
                for (auto& elem : to_erase) r->rights = erase_elem(r->rights, elem);
            }
        }
    }
    for (auto& pair : chain_pairs) {
        std::vector<std::vector<std::string>> new_rights;
        for (auto& r : grammar) {
            if (pair[1] == r->left) {
                new_rights = r->rights;
                break;
            }
        }
        for (auto& r : grammar) {
            if (pair[0] == r->left) {
                for (auto& right_part : new_rights) {
                    r->rights = insert(r->rights, right_part);
                }
                break;
            }
        }
    }
    return grammar;
}

std::vector<struct rule*> delete_unreachable(std::vector<struct rule*>& grammar) {
    std::set<std::string> reach, added;
    reach.insert("S");
    added = reach;
    while (!added.empty()) {
        added = std::set<std::string>();
        for (auto& r : grammar) {
            for (auto& symbol : reach) {
                if (r->left == symbol) {
                    for (auto& right_part : r->rights) {
                        for (auto& c : right_part) {
                            if (c[0] >= 'A' && c[0] <= 'Z' && reach.find(c) == reach.end()) added.insert(c);
                        }
                    }
                    break;
                }
            }
            for (auto& elem : added) reach.insert(elem);
        }
    }
    std::vector<struct rule*> new_grammar;
    for (auto& r : grammar) {
        if (reach.find(r->left) != reach.end()) {
            std::set<std::vector<std::string>> to_erase;
            for (auto& right_part : r->rights) {
                bool success = true;
                for (auto& symbol : right_part) {
                    if (symbol[0] >= 'A' && symbol[0] <= 'Z' && reach.find(symbol) == reach.end()) {
                        success = false;
                        break;
                    }
                }
                if (!success) to_erase.insert(right_part);
            }
            for (auto& elem : to_erase) r->rights = erase_elem(r->rights, elem);
            if (!r->rights.empty()) new_grammar.push_back(r);
        }
    }
    return new_grammar;
}

std::vector<struct rule*> delete_nongenerative(std::vector<struct rule*>& grammar) {
    std::set<std::string> gen, added;
    for (auto& r : grammar) {
        for (auto& right_part : r->rights) {
            for (auto& symbol : right_part) {
                if (symbol[0] < 'A' || symbol[0] > 'Z') {
                    gen.insert(symbol);
                }
            }
        }
    }
    added = gen;
    while (!added.empty()) {
        added = std::set<std::string>();
        for (auto& r : grammar) {
            for (auto& right_part : r->rights) {
                bool success = true;
                for (auto& symbol : right_part) {
                    if (gen.find(symbol) == gen.end()) {
                        success = false;
                        break;
                    }
                }
                if (success && gen.find(r->left) == gen.end()) {
                    gen.insert(r->left);
                    added.insert(r->left);
                    break;
                }
            }
        }
        
    }
    std::vector<struct rule*> new_grammar;
    for (auto& r : grammar) {
        if (gen.find(r->left) != gen.end()) {
            std::set<std::vector<std::string>> to_erase;
            for (auto& right_part : r->rights) {
                bool success = true;
                for (auto& symbol : right_part) {
                    if (gen.find(symbol) == gen.end()) {
                        success = false;
                        break;
                    }
                }
                if (!success) to_erase.insert(right_part);
            }
            for (auto& elem : to_erase) r->rights = erase_elem(r->rights, elem);
            if (!r->rights.empty()) new_grammar.push_back(r);
        }
    }
    return new_grammar;
}

std::vector<struct rule*> add_to_grammar(std::string& left, std::vector<std::string>& right, std::vector<struct rule*>& grammar) {
    bool exists = false;
    for (auto& elem : grammar) {
        if (elem->left == left) {
            for (auto& s : elem->rights) {
                if (s == right) {
                    exists = true;
                    break;
                }
            }
            if (!exists) elem->rights.push_back(right);
            exists = true;
        }
    }
    if (!exists) {
        struct rule* p = new (struct rule);
        p->left = left;
        p->rights.push_back(right);
        grammar.push_back(p);
    }
    return grammar;
}

std::vector<struct rule*> delete_long_rules(std::vector<struct rule*>& grammar) {
    std::vector<struct rule*> new_grammar;
    for (auto& r : grammar) {
        size_t cntr = 0;
        for (auto& right_part : r->rights) {
            size_t len = right_part.size();
            if (len >= 2) {
                std::string cur = r->left, next;
                for (size_t i = 0; i <= len - 2; i++) {
                    if (right_part[i][0] >= 'A' && right_part[i][0] <= 'Z' &&
                        right_part[i + 1][0] >= 'A' && right_part[i + 1][0] <= 'Z' && i < len - 2) {
                        std::vector<std::string> nonterm;
                        nonterm.push_back(right_part[i]);
                        nonterm.push_back(right_part[i + 1]);
                        std::string symbol = right_part[i] + right_part[i + 1];
                        new_grammar = add_to_grammar(symbol, nonterm, new_grammar);
                        right_part[i + 1] = symbol;
                    }
                    else {
                        std::stringstream s;
                        s << cur[0] << cntr << ',' << i;
                        next = s.str();
                        std::vector<std::string> new_r;
                        if (right_part[i][0] < 'A' || right_part[i][0] > 'Z') {
                            std::vector<std::string> term;
                            term.push_back(right_part[i]);
                            std::string symbol = "T" + right_part[i];
                            new_grammar = add_to_grammar(symbol, term, new_grammar);
                            new_r.push_back(symbol);
                        }
                        else new_r.push_back(right_part[i]);
                        if (i < len - 2) new_r.push_back(next);
                        else {
                            if (right_part[i + 1][0] < 'A' || right_part[i + 1][0] > 'Z') {
                                std::vector<std::string> term;
                                term.push_back(right_part[i + 1]);
                                std::string symbol = "T" + right_part[i + 1];
                                new_grammar = add_to_grammar(symbol, term, new_grammar);
                                new_r.push_back(symbol);
                            }
                            else new_r.push_back(right_part[i + 1]);
                        }
                        new_grammar = add_to_grammar(cur, new_r, new_grammar);
                        cur = next;
                    }
                    
                }
            }
            else new_grammar = add_to_grammar(r->left, right_part, new_grammar);
            cntr++;
        }
    }
    return new_grammar;
}

std::string cyk(std::string& word, std::vector<struct rule*>& grammar) {
    std::map<std::string, size_t> to_int;
    for (auto& r : grammar) {
        to_int.insert(std::pair{ r->left, to_int.size() });
        for (auto& right_part : r->rights) {
            for (auto& symbol : right_part) {
                to_int.insert(std::pair{ symbol, to_int.size() });
            }
        }
    }
    std::map<std::vector<size_t>, std::set<size_t>> from_nonterms;
    std::map<size_t, std::set<size_t>> terms;
    std::set<std::vector<size_t>> nt_keys;
    std::set<size_t> t_keys;
    for (auto& r : grammar) {
        size_t int_left = to_int[r->left];
        for (auto& right_part : r->rights) {
            if (right_part.size() == 2) {
                std::vector<size_t> v;
                v.push_back(to_int[right_part[0]]);
                v.push_back(to_int[right_part[1]]);
                from_nonterms[v].insert(int_left);
                nt_keys.insert(v);
            }
            else {
                size_t t = to_int[right_part[0]];
                terms[t].insert(int_left);
                t_keys.insert(t);
            }
        }
    }        
    std::vector<std::set<size_t>> table;
    size_t n = word.length();
    for (size_t i = 0; i < n; i++) {
        std::set<size_t> non_terms;
        std::string letter;
        letter.push_back(word[i]);
        if (to_int.find(letter) != to_int.end()) {
            size_t int_letter = to_int[letter];
            for (auto& key : t_keys) {
                if (key == int_letter)
                {
                    for (auto& t : terms[key]) {
                        non_terms.insert(t);
                    }
                    break;
                }
            }
        }
        else return "NO";
        if (non_terms.empty()) return "NO";
        table.push_back(non_terms);
    }
    for (size_t i = 1; i <= n - 1; i++) {
        for (size_t j = 0; j < n - i; j++) {
            std::set<size_t> non_terms;
            size_t pos = (n + n - (i - 1)) * i / 2 + j;
            size_t first_vert_shift = n - i + 1;
            size_t second_vert_ind = 0, second_hor_shift = j + i;
            size_t k = pos - first_vert_shift;
            while (k >= 0) {
                size_t second_pos = (n + n - (second_vert_ind - 1)) * second_vert_ind / 2 + second_hor_shift;
                for (auto& key : nt_keys) {
                    if (table[k].find(key[0]) != table[k].end() &&
                        table[second_pos].find(key[1]) != table[second_pos].end()) {
                        for (auto& nt : from_nonterms[key]) {
                            non_terms.insert(nt);
                        }
                    }
                }
                first_vert_shift++;
                second_vert_ind++;
                second_hor_shift--;
                if (k >= first_vert_shift) k = k - first_vert_shift;
                else break;
            }
            table.push_back(non_terms);
        }
    }
    std::set<size_t> result = table[(n + 1) * n / 2 - 1];
    if (result.find(to_int["S"]) != result.end()) return "YES";
    return "NO";
}

std::string check_if_eps(std::vector<struct rule*>& grammar) {
    std::set<std::string> eps_nonterm, new_set;
    std::vector<struct rule*> new_grammar;
    for (auto& r : grammar) {
        for (auto& right : r->rights) {
            if (*right.begin() == "_") {
                eps_nonterm.insert(r->left);
                r->rights = erase_elem(r->rights, right);
                break;
            }
        }
        if (!r->rights.empty()) new_grammar.push_back(r);
    }
    grammar = new_grammar;
    new_set = eps_nonterm;
    while (!new_set.empty()) {
        new_set = std::set<std::string>();
        for (auto& r : grammar) {
            if (eps_nonterm.find(r->left) == eps_nonterm.end()) {
                for (auto& right : r->rights) {
                    std::set<std::string> nonterm;
                    bool success = true;
                    for (auto& symbol : right) {
                        if ((symbol[0] < 'A') || (symbol[0] > 'Z')) {
                            success = false;
                            break;
                        }
                        else if (eps_nonterm.find(symbol) == eps_nonterm.end()) {
                            success = false;
                            break;
                        }
                        else nonterm.insert(r->left);
                    }
                    if (success) {
                        for (auto& elem : nonterm) {
                            new_set.insert(elem);
                            eps_nonterm.insert(elem);
                        }
                        break;
                    }

                }
            }
        }
    }
    if (eps_nonterm.find("S") != eps_nonterm.end()) return "YES";
    return "NO";
}

int main(void)
{
    std::ifstream infile("cyk.in");
    std::ofstream outfile("cyk.out");
    char left_part;
    std::string word, right_part;
    std::vector<struct rule*> grammar;
    if (!infile.eof()) infile >> word;
    while (!infile.eof()) {
        infile >> left_part;
        infile >> right_part;
        bool exists = false;
        for (auto &elem : grammar) {
            std::string str_left_part;
            str_left_part.push_back(left_part);
            if (elem->left == str_left_part) {
                exists = true;
                elem->rights.push_back(str_to_vec(right_part));
            }
        }
        if (!exists) {
            struct rule* p = new (struct rule);
            p->left = left_part;
            p->rights.push_back(str_to_vec(right_part));
            grammar.push_back(p);
        }     
    }
    if (word == "_") {
        outfile << check_if_eps(grammar);
        return 0;
    }
    grammar = delete_long_rules(grammar);
    grammar = delete_epsilon(grammar);
    grammar = delete_chains(grammar);
    grammar = delete_unreachable(grammar);
    grammar = delete_nongenerative(grammar);
    std::string res = cyk(word, grammar);
    outfile << res;
    return 0;
}