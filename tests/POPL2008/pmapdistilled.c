#define NVPAGES		2	/* number of virtual pages per environment */
#define NPPAGES		1	/* number of physical pages per system */
#define NULL		0

/* in real life, NVPAGES >> NPPAGES */

void __BLAST_assert(int p) {
	if (p == 0) {
BERROR: goto BERROR;
        }
}
void assert(int p) {
	if (p == 0) {
ERROR: goto ERROR;
        }
}


typedef struct env {
    int env_mypp;		/* physical pageno of page containing env */
    int env_pgdir[NVPAGES];	/* "page table" */
    struct env *env_prev;	/* links in 'envs' set */
    struct env *env_next;
} env_t;

int pages[NPPAGES];		/* physical page refcounts, initially 0 */
int page_protected[NPPAGES];	/* true for protected pages, initially 0 */
env_t *envs = NULL;		/* set of all environments */

int page_getfree(void)		/* return free page */
{
    int i;
    for (i = 0; i < NPPAGES; i++)
	if (pages[i] == 0)
	    return i;
    return -1;
}

int page_free(int ppno)		/* is this page protected? */
{
    assert(ppno >= 0 && ppno < NPPAGES);
    assert(pages[ppno] > 0 || page_protected[ppno] == 0);
    return pages[ppno] == 0;
}

int is_page_protected(int ppno)	/* is this page protected from user access? */
{
    assert(ppno >= 0 && ppno < NPPAGES);
    assert(page_protected[ppno] == 0 || pages[ppno] > 0);
    return page_protected[ppno] != 0;
}

void page_decref(int ppno)	/* decrement page reference count */
{
    assert(!page_free(ppno));
    pages[ppno]--;
}

void env_check(env_t *env)	/* check environment's validity */
{
    int i, found;
    env_t *walk;
    /* page map is protected */
    assert(is_page_protected(env->env_mypp));
    /* page directory */
    for (i = 0; i < NVPAGES; i++)
	if (env->env_pgdir[i] >= 0) {
	    assert(!page_free(env->env_pgdir[i]));
	    assert(!is_page_protected(env->env_pgdir[i]));
	}
    /* on list */
    for (walk = envs, found = 0; walk; walk = walk->env_next)
	if (walk == env)
	    found = 1;
    assert(found);
}

void mem_check(void)		/* check validity of all of memory */
{
    int i, lpages[NPPAGES];
    env_t *walk;

    for (i = 0; i < NPPAGES; i++)
	lpages[i] = 0;

    for (walk = envs; walk; walk = walk->env_next) {
	assert(is_page_protected(walk->env_mypp));
	lpages[walk->env_mypp]++;
	for (i = 0; i < NVPAGES; i++)
	    if (walk->env_pgdir[i] >= 0) {
		assert(!is_page_protected(walk->env_pgdir[i]));
		lpages[walk->env_pgdir[i]]++;
	    }
    }

    for (i = 0; i < NPPAGES; i++) {
	assert(lpages[i] == pages[i]);
	assert(lpages[i] > 0 || page_protected[i] == 0);
    }
}

/* BEGIN USER VISIBLE OPERATIONS */

env_t *env_alloc(void)		/* allocate new environment structure */
{
    env_t *env;
    int i, env_pp = page_getfree();
    if (env_pp < 0)
	return NULL;
    
    env = (env_t *) malloc(sizeof(env_t));
    env->env_mypp = env_pp;
    for (i = 0; i < NVPAGES; i++)
	env->env_pgdir[i] = -1;

    /* put on list */
    env->env_next = envs;
    env->env_prev = NULL;
    if (envs)
	envs->env_prev = env;
    envs = env;
    
    pages[env_pp]++;
    page_protected[env_pp] = 1;
    env_check(env);
    mem_check();
    return env;
}

void env_free(env_t *env)	/* free environment structure */
{
    int i;
    env_check(env);
    
    for (i = 0; i < NVPAGES; i++)
	if (env->env_pgdir[i] >= 0)
	    page_decref(env->env_pgdir[i]);
    page_protected[env->env_mypp] = 0;
    page_decref(env->env_mypp);
    assert(page_free(env->env_mypp));

    if (env->env_next)
	env->env_next->env_prev = env->env_prev;
    if (env->env_prev)
	env->env_prev->env_next = env->env_next;
    else
	envs = env->env_next;
    
    free(env);
    mem_check();
}

int page_alloc(env_t *env, int vp)	/* allocate page in env pgdir */
{
    int pp;
    assert(vp >= 0 && vp < NVPAGES);
    
    pp = page_getfree();
    if (pp < 0)
	return -1;
    
    if (env->env_pgdir[vp] >= 0)
	page_decref(env->env_pgdir[vp]);
    env->env_pgdir[vp] = pp;
    pages[pp]++;
    
    env_check(env);
    mem_check();
    return 0;
}

void page_unmap(env_t *env, int vp)	/* free page in env pgdir */
{
    assert(vp >= 0 && vp < NVPAGES);
    if (env->env_pgdir[vp] >= 0)
	page_decref(env->env_pgdir[vp]);
    env_check(env);
    mem_check();
}

int page_map(env_t *srcenv, int srcvp,
	     env_t *dstenv, int dstvp)	/* shared memory */
{
    assert(srcvp >= 0 && srcvp < NVPAGES);
    assert(dstvp >= 0 && dstvp < NVPAGES);
    env_check(srcenv);
    env_check(dstenv);
    
    if (srcenv->env_pgdir[srcvp] < 0)
	return -1;
    
    if (dstenv->env_pgdir[dstvp] >= 0)
	page_decref(dstenv->env_pgdir[dstvp]);
    dstenv->env_pgdir[dstvp] = srcenv->env_pgdir[srcvp];
    pages[dstenv->env_pgdir[dstvp]]++;
    
    env_check(dstenv);
    mem_check();
    return 0;
}

int main(int argc, char *argv[])
{
    /* Any sequence of operations env_alloc(), env_free() (of a valid
       environment returned by env_alloc()), page_alloc(), page_unmap(), and
       page_map() (with valid arguments) should preserve env_check() for all
       environments and mem_check(). */
    env_t *e = env_alloc();
    if (e!=0) { env_check(e);
    	        env_free(e); }
}
