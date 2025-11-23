typedef long Align;

union header {
  struct {
    union header *ptr;
    unsigned size;
  } s;
  Align x;
};

typedef union header Header;

static const int __malloc_size = 128;

static int __malloc_buffer[__malloc_size];

static Header *__freep;

void
free(void *ap)
{
  Header *bp, *p;

  bp = (Header*)ap - 1;
  for(p = __freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
    if(p >= p->s.ptr && (bp > p || bp < p->s.ptr))
      break;
  if(bp + bp->s.size == p->s.ptr){
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  } else
    bp->s.ptr = p->s.ptr;
  if(p + p->s.size == bp){
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else
    p->s.ptr = bp;
  __freep = p;
}

extern void print_i32(int);

// TODO: fix out-of-memory detection
void*
malloc(unsigned nbytes)
{
  Header *p, *prevp;
  unsigned nunits;

  nunits = (nbytes + sizeof(Header) - 1)/sizeof(Header) + 1;
  if((prevp = __freep) == 0){
    Header *buf = (Header*)(&__malloc_buffer[0]);
    buf->s.ptr = __freep = prevp = buf;
    buf->s.size = (sizeof(int) * __malloc_size) / sizeof(Header);
  }
  for(p = prevp->s.ptr; ; prevp = p, p = p->s.ptr){
    if(p->s.size >= nunits){
      if(p->s.size == nunits)
        prevp->s.ptr = p->s.ptr;
      else {
        p->s.size -= nunits;
        p += p->s.size;
        p->s.size = nunits;
      }
      __freep = prevp;
      return (void*)(p + 1);
    }
    if(p == __freep)
      return 0;
  }
}
