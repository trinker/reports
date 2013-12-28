function regImg(x, w, h)
{
w = typeof w !== 'undefined' ? w : '32px';
h = typeof h !== 'undefined' ? h : w;
x.style.height=h;
x.style.width=w;
}

function bigImg(x, w, h)
{
w = typeof w !== 'undefined' ? w : '200px';
h = typeof h !== 'undefined' ? h : w;
x.style.height=h;
x.style.width=w;
}


function myFunction(x)
{
alert(x);
}
