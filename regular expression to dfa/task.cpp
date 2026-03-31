#include "api.hpp"
#include <string>
#include <set>

struct nfl {
	bool nullable;
	std::set<int>* firstpos;
	std::set<int>* lastpos;
};

struct tree {
	char root;
	nfl* info;
	struct tree* left;
	struct tree* right;
};

struct followpos {
	char symbol = ' ';
	std::set<int>* follow = NULL;
};

std::string set_to_str(std::set<int>& s) {
	std::string res;
	for (auto elem : s) {
		res += std::to_string(elem);
	}
	return res;
}

std::set<int>* set_union(std::set<int>& s1, std::set<int>& s2) {
	std::set<int>* s = new std::set<int>;
	for (auto elem : s1) {
		s->insert(elem);
	}
	for (auto elem : s2) {
		s->insert(elem);
	}
	return s;
}

std::string no_external_brackets(std::string s) {
	size_t br_cntr = 0, i = s.length() - 1;
	while (i > 0) {
		if (s[i] == ')') br_cntr -= 1;
		else if (s[i] == '(') br_cntr += 1;
		if (!br_cntr) return s;
		i--;
	}
	return s.substr(1, s.length() - 2);
}

int pos = 0, final_pos = 0;
struct followpos* fpos = new struct followpos[1000];

tree* syn_tree(std::string s, Alphabet& sigma) {
	if (s.length() <= 1) {
		struct tree* leaf = new struct tree;
		if (s.empty()) leaf->root = ' ';
		else {
			leaf->root = s[0];
			pos++;
			if (s[0] == '#') final_pos = pos;
			fpos[pos].symbol = leaf->root;
		}
		leaf->left = NULL;
		leaf->right = NULL;
		leaf->info = new struct nfl;
		if (s.empty()) {
			leaf->info->nullable = true; 
			leaf->info->firstpos = NULL; 
			leaf->info->lastpos = NULL;
		}
		else {
			leaf->info->nullable = false;
			leaf->info->firstpos = new std::set<int>;
			leaf->info->firstpos->insert(pos);
			leaf->info->lastpos = new std::set<int>;
			leaf->info->lastpos->insert(pos);
		}
		return leaf;
	}
	size_t i = s.length() - 1;
	if (s[i] == ')') s = no_external_brackets(s);
	i = s.length() - 1;
	char min_priority_op = ' ';
	size_t min_priority_ind = 0, bracket = 0;
	while (i > 0) {
		if (s[i] == ')') bracket += 1;
		else if (s[i] == '(') {
			bracket -= 1;
			if (!bracket && i > 0 && 
				(s[i - 1] == '*' || s[i - 1] == ')' || sigma.has_char(s[i - 1]))) {
				if (min_priority_op == ' ' || min_priority_op == '*') {
					min_priority_op = '.';
					min_priority_ind = i;
				}
			}
		} 
		else if (!bracket && s[i] == '|') {
			min_priority_op = '|';
			min_priority_ind = i;
			i = 1;
		} 
		else if (!bracket && s[i] == '*' && min_priority_op == ' ') {
			min_priority_op = '*';
			min_priority_ind = i;
		}
		else if (!bracket && (s[i] == '#' || sigma.has_char(s[i]))) {
			if ((i > 0 && (s[i - 1] == '*' || s[i - 1] == ')' || sigma.has_char(s[i - 1]))) &&
				(min_priority_op == ' ' || min_priority_op == '*')) {
				min_priority_op = '.';
				min_priority_ind = i;
			}
		}
		i--;
	}
	
	if (s[0] == '|') {

		min_priority_op = '|';
		min_priority_ind = 0;
	}
	i = min_priority_ind;
	
	std::string s_left, s_right;
	s_left = s.substr(0, i);
	if (min_priority_op == '.') s_right = s.substr(i);
	else s_right = s.substr(i + 1);
	struct tree* t = new struct tree;
	t->root = min_priority_op;
	t->left = syn_tree(s_left, sigma);
	if (min_priority_op == '*') t->right = NULL;
	else t->right = syn_tree(s_right, sigma);
	
	t->info = new struct nfl;
	if (t->root == '*') {
		t->info->nullable = true;
		t->info->firstpos = t->left->info->firstpos;
		t->info->lastpos = t->left->info->lastpos;
		for (auto i : *t->left->info->lastpos) {
			if (fpos[i].follow == NULL) fpos[i].follow = new std::set<int>;
			fpos[i].follow = set_union(*fpos[i].follow, *t->left->info->firstpos);
		}
	}
	else if (t->root == '|') {
		t->info->nullable = t->left->info->nullable || t->right->info->nullable;
		
		if (t->left->info->firstpos == NULL) t->info->firstpos = t->right->info->firstpos;
		else if (t->right->info->firstpos == NULL) t->info->firstpos = t->left->info->firstpos;
		else t->info->firstpos = set_union(*t->left->info->firstpos, *t->right->info->firstpos);

		if (t->left->info->lastpos == NULL) t->info->lastpos = t->right->info->lastpos;
		else if (t->right->info->lastpos == NULL) t->info->lastpos = t->left->info->lastpos;
		else t->info->lastpos = set_union(*t->left->info->lastpos, *t->right->info->lastpos);
	}
	else if (t->root == '.') {
		bool left_nullable = t->left->info->nullable;
		bool right_nullable = t->right->info->nullable;
		t->info->nullable = left_nullable && right_nullable;

		if (left_nullable) t->info->firstpos = set_union(*t->left->info->firstpos, *t->right->info->firstpos);
		else t->info->firstpos = t->left->info->firstpos;

		if (right_nullable) t->info->lastpos = set_union(*t->left->info->lastpos, *t->right->info->lastpos);
		else t->info->lastpos = t->right->info->lastpos;

		for (auto i : *t->left->info->lastpos) {
			if (fpos[i].follow == NULL) fpos[i].follow = new std::set<int>;
			fpos[i].follow = set_union(*fpos[i].follow, *t->right->info->firstpos);
		}
	}
	return t;
}

void delete_tree(struct tree* st) {
	if (st->left == NULL && st->right == NULL) {
		delete st->info;
	}
	else if (st->left == NULL) {
		delete_tree(st->right);
	}
	else if (st->right == NULL) {
		delete_tree(st->left);
	}
	else {
		delete_tree(st->left);
		delete_tree(st->right);
	}
	delete st;
}

DFA re2dfa(const std::string &s) {
	if (s.empty()) {
		std::string emp;
		for (char c = 'A'; c <= 'Z'; c++) emp += c;
		for (char c = 'a'; c <= 'z'; c++) emp += c;
		for (char c = '0'; c <= '9'; c++) emp += c;
		DFA res = DFA(Alphabet(emp));
		res.create_state("Start");
		res.set_initial("Start");
		res.make_final("Start");
		return res;
	}
	Alphabet sigma = Alphabet(s);
	DFA res = DFA(sigma);
	struct tree* tr = syn_tree('(' + s + ')' + '#', sigma);
	
	std::set<int>* Q = new std::set<int>[1001], * marked = new std::set<int>[1001];
	Q[0] = *tr->info->firstpos;
	std::string q0 = set_to_str(Q[0]);
	res.create_state(q0);
	res.set_initial(q0);
	if (Q[0].find(pos) != Q[0].end()) res.make_final(q0);
	int Q_size = 1, marked_size = 0, iQ = 0, im = 0, num_of_new = 0;
	std::set<int> R;
	while (Q_size != marked_size) {
		for (int j = iQ; j < Q_size; j++) {
			marked[im] = Q[j];
			im += 1;
			marked_size += 1;
		}
		num_of_new = 0;
		for (int j = iQ; j < Q_size; j++) {
			R = Q[j];
			for (auto a : sigma.to_string()) {
				std::set<int> S;
				for (int p = 1; p < pos; p++) {
					if (fpos[p].symbol == a) {
						for (auto elem : R) {
							if (elem == p) {
								S = *set_union(S, *fpos[p].follow);
							}
						}
					}
				}
				if (!S.empty()) {
					std::string state = set_to_str(S);
					bool found = false;
					for (int j = 0; j < Q_size + num_of_new; j++) {
						if (S == Q[j]) {
							found = true;
							break;
						}
					}
					if (!found) {
						res.create_state(state);
						if (S.find(pos) != S.end()) res.make_final(state);
						Q[Q_size + num_of_new] = S;
						num_of_new += 1;
					}
					res.set_trans(set_to_str(R), a, state);
				}
			}
		}
		Q_size += num_of_new;
		iQ = im;
	}

	delete_tree(tr);
	delete []Q;
	delete []marked;
	delete []fpos;
	return res;
}
