#include "api.hpp"
#include <string>
#include <set>
#include <vector>

struct regexp {
	std::string from_state;
	std::string expr;
	std::string to_state;
};

std::vector<struct regexp*> add_trans(std::vector<struct regexp*> v, struct regexp* new_elem) {
	std::vector<struct regexp*> new_v;
	if (v.empty()) {
		new_v.push_back(new_elem);
		return new_v;
	}
	bool added = false;
	for (auto elem : v) {
		if (elem->from_state == new_elem->from_state &&
			elem->to_state == new_elem->to_state) {
			struct regexp* trans = new struct regexp;
			trans->from_state = elem->from_state;
			trans->to_state = elem->to_state;
			trans->expr = elem->expr + '|' + new_elem->expr;
			new_v.push_back(trans);
			added = true;
		}
		else new_v.push_back(elem);
	}
	if (!added) new_v.push_back(new_elem);
	return new_v;
}

std::string add_brackets(std::string& expr) {
	bool need_brackets = false;
	size_t br = 0, i = 0;
	while (i < expr.length()) {
		if (expr[i] == '|' && !br) {
			need_brackets = true;
			i = expr.length();
		}
		else if (expr[i] == '(') br += 1;
		else if (expr[i] == ')') br -= 1;
		i++;
	}
	if (need_brackets) return '(' + expr + ')';
	else return expr;
}

std::string dfa2re(DFA& d) {
	std::set<std::string> states = d.get_states(), finals = d.get_final_states(), exclude;
	std::string symbols = d.get_alphabet().to_string();
	std::vector<struct regexp*> all_trans;
	for (auto& from : states) {
		if (!d.is_initial(from) && !d.is_final(from)) exclude.insert(from);
		for (auto s : symbols) {
			if (d.has_trans(from, s)) {
				struct regexp* r = new struct regexp;
				r->from_state = from;
				r->expr = s;
				r->to_state = d.get_trans(from, s);
				all_trans.push_back(r);
			}
		}
	}
	bool finish_flag = false;
	if (exclude.empty()) {
		exclude = finals;
		auto first = exclude.begin();
		if (d.is_initial(*first)) exclude.erase(first);
		finals = std::set<std::string>();
		if (exclude.empty()) {
			exclude.insert(d.get_initial_state());
			finish_flag = true;
		}
	}
	while (!exclude.empty()) {
		auto it = exclude.begin();
		bool loop = false;
		std::string loop_expr;
		std::vector<struct regexp*> in, out, new_all_trans;
		for (auto trans : all_trans) {
			std::string from = trans->from_state, to = trans->to_state;
			if (from == *it && from == to) {
				loop = true;
				if (loop_expr.empty()) loop_expr = trans->expr;
				else loop_expr = loop_expr + '|' + trans->expr;
				
			}
			else if (from == *it) out.push_back(trans);
			else if (to == *it) in.push_back(trans);
			else new_all_trans.push_back(trans);
		}
		if (d.is_final(*it)) {
			struct regexp* trans = new struct regexp;
			trans->from_state = *it;
			trans->expr = std::string();
			trans->to_state = std::string();
			out = add_trans(out, trans);
		}
		if (d.is_initial(*it)) {
			struct regexp* trans = new struct regexp;
			trans->from_state = std::string();
			trans->expr = std::string();
			trans->to_state = *it;
			in = add_trans(in, trans);
		}
		for (auto i : in) {
			for (auto o : out) {
				struct regexp* trans = new struct regexp;
				trans->from_state = i->from_state;
				std::string i_expr = i->expr, o_expr = o->expr;
				if (!i_expr.empty() && !o_expr.empty() || loop) {
					i_expr = add_brackets(i->expr);
					o_expr = add_brackets(o->expr);
				}
				if (loop && loop_expr.length() == 1) {
					trans->expr = i_expr + loop_expr + '*' + o_expr;
				}
				else if (loop) {
					trans->expr = i_expr + '(' + loop_expr + ')' + '*' + o_expr;
				}
				else {
					trans->expr = i_expr + o_expr;
				}
				trans->to_state = o->to_state;
				new_all_trans = add_trans(new_all_trans, trans);
			}
		}
		all_trans = new_all_trans;
		exclude.erase(it);
		if (exclude.empty() && !finals.empty()) {
			exclude = finals;
			auto first = exclude.begin();
			if (d.is_initial(*first)) exclude.erase(first);
			finals = std::set<std::string>();
		}
		if (exclude.empty() && finals.empty() && !finish_flag) {
			exclude.insert(d.get_initial_state());
			finish_flag = true;
		}
	}
	struct regexp* trans = *all_trans.begin();
	std::string res = trans->expr;
	return res;
}
