#include "util.h"

#include <stdarg.h>

#ifdef WIN32
#include <windows.h> /* Sleep */
#else
#include <time.h> /* nanosleep */
#endif

void sleep_secs(double secs) {
#if defined(_WIN32) || defined(WIN32)
	Sleep(secs * 1000.0);
#else
	struct timespec ts;
	ts.tv_sec = (time_t)secs;
	ts.tv_nsec = (secs - (double)ts.tv_sec) * 1000000000.0;
	nanosleep(&ts, NULL);
#endif
}

char errbuf[ERRSZ];
bool err;
size_t err_ln, err_col;

static intmax_t stoimax_digits[256] = {
	[0]    = -1, [1]    = -1, [2]    = -1, [3]    = -1, [4]    = -1, [5]    = -1, [6]    = -1, [7]    = -1,
	[8]    = -1, [9]    = -1, [10]   = -1, [11]   = -1, [12]   = -1, [13]   = -1, [14]   = -1, [15]   = -1,
	[16]   = -1, [17]   = -1, [18]   = -1, [19]   = -1, [20]   = -1, [21]   = -1, [22]   = -1, [23]   = -1,
	[24]   = -1, [25]   = -1, [26]   = -1, [27]   = -1, [28]   = -1, [29]   = -1, [30]   = -1, [31]   = -1,
	[32]   = -1, [33]   = -1, [34]   = -1, [35]   = -1, [36]   = -1, [37]   = -1, [38]   = -1, [39]   = -1,
	[40]   = -1, [41]   = -1, [42]   = -1, [43]   = -1, [44]   = -1, [45]   = -1, [46]   = -1, [47]   = -1,
	['0']  =  0, ['1']  =  1, ['2']  =  2, ['3']  =  3, ['4']  =  4, ['5']  =  5, ['6']  =  6, ['7']  =  7,
	['8']  =  8, ['9']  =  9, [58]   = -1, [59]   = -1, [60]   = -1, [61]   = -1, [62]   = -1, [63]   = -1,
	[64]   = -1, ['A']  = 10, ['B']  = 11, ['C']  = 12, ['D']  = 13, ['E']  = 14, ['F']  = 15, [71]   = -1,
	[72]   = -1, [73]   = -1, [74]   = -1, [75]   = -1, [76]   = -1, [77]   = -1, [78]   = -1, [79]   = -1,
	[80]   = -1, [81]   = -1, [82]   = -1, [83]   = -1, [84]   = -1, [85]   = -1, [86]   = -1, [87]   = -1,
	[88]   = -1, [89]   = -1, [90]   = -1, [91]   = -1, [92]   = -1, [93]   = -1, [94]   = -1, [95]   = -1,
	[96]   = -1, ['a']  = 10, ['b']  = 11, ['c']  = 12, ['d']  = 13, ['e']  = 14, ['f']  = 15, [103]  = -1,
	[104]  = -1, [105]  = -1, [106]  = -1, [107]  = -1, [108]  = -1, [109]  = -1, [110]  = -1, [111]  = -1,
	[112]  = -1, [113]  = -1, [114]  = -1, [115]  = -1, [116]  = -1, [117]  = -1, [118]  = -1, [119]  = -1,
	[120]  = -1, [121]  = -1, [122]  = -1, [123]  = -1, [124]  = -1, [125]  = -1, [126]  = -1, [127]  = -1,
	[128]  = -1, [129]  = -1, [130]  = -1, [131]  = -1, [132]  = -1, [133]  = -1, [134]  = -1, [135]  = -1,
	[136]  = -1, [137]  = -1, [138]  = -1, [139]  = -1, [140]  = -1, [141]  = -1, [142]  = -1, [143]  = -1,
	[144]  = -1, [145]  = -1, [146]  = -1, [147]  = -1, [148]  = -1, [149]  = -1, [150]  = -1, [151]  = -1,
	[152]  = -1, [153]  = -1, [154]  = -1, [155]  = -1, [156]  = -1, [157]  = -1, [158]  = -1, [159]  = -1,
	[160]  = -1, [161]  = -1, [162]  = -1, [163]  = -1, [164]  = -1, [165]  = -1, [166]  = -1, [167]  = -1,
	[168]  = -1, [169]  = -1, [170]  = -1, [171]  = -1, [172]  = -1, [173]  = -1, [174]  = -1, [175]  = -1,
	[176]  = -1, [177]  = -1, [178]  = -1, [179]  = -1, [180]  = -1, [181]  = -1, [182]  = -1, [183]  = -1,
	[184]  = -1, [185]  = -1, [186]  = -1, [187]  = -1, [188]  = -1, [189]  = -1, [190]  = -1, [191]  = -1,
	[192]  = -1, [193]  = -1, [194]  = -1, [195]  = -1, [196]  = -1, [197]  = -1, [198]  = -1, [199]  = -1,
	[200]  = -1, [201]  = -1, [202]  = -1, [203]  = -1, [204]  = -1, [205]  = -1, [206]  = -1, [207]  = -1,
	[208]  = -1, [209]  = -1, [210]  = -1, [211]  = -1, [212]  = -1, [213]  = -1, [214]  = -1, [215]  = -1,
	[216]  = -1, [217]  = -1, [218]  = -1, [219]  = -1, [220]  = -1, [221]  = -1, [222]  = -1, [223]  = -1,
	[224]  = -1, [225]  = -1, [226]  = -1, [227]  = -1, [228]  = -1, [229]  = -1, [230]  = -1, [231]  = -1,
	[232]  = -1, [233]  = -1, [234]  = -1, [235]  = -1, [236]  = -1, [237]  = -1, [238]  = -1, [239]  = -1,
	[240]  = -1, [241]  = -1, [242]  = -1, [243]  = -1, [244]  = -1, [245]  = -1, [246]  = -1, [247]  = -1,
	[248]  = -1, [249]  = -1, [250]  = -1, [251]  = -1, [252]  = -1, [253]  = -1, [254]  = -1, [255]  = -1,
};

void set_err(const char *fmt, ...) {
	err = true;
	va_list va;
	va_start(va, fmt);
	vsnprintf(errbuf, ERRSZ, fmt, va);
	va_end(va);
}

#define XMALLOC_ERR "Failed to allocate %zu bytes: Out of memory\n"

void *xmalloc(size_t size) {
	void *ret = malloc(size);
	if (!ret) {
		fprintf(stderr, XMALLOC_ERR, size);
		abort();
	}
	return ret;
}

void *xrealloc(void *ptr, size_t size) {
	void *ret = realloc(ptr, size);
	if (!ret) {
		fprintf(stderr, XMALLOC_ERR, size);
		abort();
	}
	return ret;
}

Pool *pool_new(size_t init_cap) {
	Pool *p = xmalloc(sizeof(Pool) + init_cap);
	p->len = 0;
	p->cap = init_cap;
	p->data = p + 1;
	p->next = NULL;
	return p;
}

void pool_term(Pool *p) {
	for (Pool *i = p; i != NULL;) {
		Pool *next = i->next;
		free(i);
		i = next;
	}
}

void *pool_alloc(Pool *p, size_t bytes) {
	for (Pool *i = p;; i = i->next) {
		if (i->len + bytes < i->cap) {
			void *ret = (uint8_t*)i->data + i->len;
			i->len += bytes;
			return ret;
		}
		if (!i->next) {
			i->next = pool_new(bytes + i->cap * 2);
			i->next->len = bytes;
			return i->next->data;
		}
	}
}

char *sndup(const char *s, size_t n) {
	char *ret = xmalloc(n+1);
	if (ret) {
		memcpy(ret, s, n);
		ret[n] = 0;
	}
	return ret;
}

char *psndup(Pool *p, const char *s, size_t n) {
	char *ret = pool_alloc(p, n+1);
	memcpy(ret, s, n);
	ret[n] = 0;
	return ret;
}

intmax_t stoimax(const char *s, size_t n, size_t base, ssize_t *endpos) {
	for (size_t i = 0; i < n; i++) { if (s[i] == 0) { n = i; break; } }
	intmax_t res = 0;
	intmax_t order = 1;
	for (ssize_t i = n - 1; i >= 0; i--) {
		intmax_t dig = stoimax_digits[(size_t)s[i]];
		if (dig == -1 || (size_t)dig >= base) {
			if (endpos)
				*endpos = i;
			return 0;
		}
		res += order * dig;
		order *= base;
	}
	if (endpos)
		*endpos = -1;
	return res;
}

double stod(const char *s, size_t n, ssize_t *endpos) {
	for (size_t i = 0; i < n; i++) { if (s[i] == 0) { n = i; break; } }
	double res = 0.0;
	double order = 1.0;
	size_t point_pos = n;
	for (size_t i = 0; i < n; i++) { if (s[i] == '.') { point_pos = i; break; } }
	for (ssize_t i = point_pos - 1; i >= 0; i--) {
		if (!IS_NUM(s[i])) {
			if (endpos)
				*endpos = i;
			return 0.0;
		}
		double dig = s[i] - '0';
		res += order * dig;
		order *= 10.0;
	}
	order = 0.1;
	for (size_t i = point_pos + 1; i < n; i++) {
		if (!IS_NUM(s[i])) {
			if (endpos)
				*endpos = i;
			return 0.0;
		}
		double dig = s[i] - '0';
		res += order * dig;
		order *= 0.1;
	}
	if (endpos)
		*endpos = -1;
	return res;
}

const char *unescape_char(char c) {
	switch (c) {
		case '\a':   return "\\a";
		case '\b':   return "\\b";
		case '\033': return "\\e";
		case '\f':   return "\\f";
		case '\n':   return "\\n";
		case '\r':   return "\\r";
		case '\t':   return "\\t";
		case '\v':   return "\\v";
		case '\\':   return "\\\\";
		case '\'':   return "\\'";
		case '"':    return "\\\"";
		default:     return NULL;
	}
}

char *mreadfile(FILE *fp) {
	if (fseek(fp, 0l, SEEK_END) == -1)
		return NULL;
	long size = ftell(fp);
	if (size == -1)
		return NULL;
	rewind(fp);
	char *buf = malloc(size + 1);
	if (!buf)
		return NULL;
	size_t read = fread(buf, size, 1, fp);
	if (read != 1) {
		free(buf);
		return NULL;
	}
	buf[size] = 0;
	return buf;
}

uint32_t fnv1a32(const void *data, size_t n) {
	uint32_t res = 2166136261u;
	for (size_t i = 0; i < n; i++) {
		res ^= ((uint8_t*)data)[i];
		res *= 16777619u;
	}
	return res;
}
