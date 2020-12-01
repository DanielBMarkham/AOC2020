open System
let newData =  seq{1511 ; 1344 ; 1925 ; 1970 ; 1864 ; 1951 ; 1557 ; 1984 ; 1743 ; 1526 ; 1972 ; 1945 ; 1969 ; 1760 ; 2008 ; 1592 ; 736 ; 1963 ; 1994 ; 2009 ; 1777 ; 1856 ; 1899 ; 1926 ; 1850 ; 687 ; 2005 ; 1094 ; 1949 ; 1326 ; 2002 ; 1805 ; 1493 ; 1341 ; 1828 ; 1778 ; 1767 ; 1364 ; 1973 ; 1768 ; 1929 ; 1377 ; 2000 ; 1726 ; 1913 ; 2001 ; 1574 ; 1859 ; 1793 ; 1957 ; 1959 ; 1388 ; 1593 ; 1392 ; 724 ; 1962 ; 1999 ; 252 ; 1982 ; 1662 ; 1892 ; 1610 ; 1343 ; 1831 ; 1862 ; 1991 ; 1394 ; 1946 ; 1935 ; 1986 ; 1911 ; 1358 ; 1322 ; 1956 ; 1988 ; 1758 ; 1490 ; 1998 ; 1744 ; 1844 ; 1294 ; 1764 ; 1543 ; 1560 ; 1562 ; 1747 ; 1870 ; 1292 ; 1989 ; 1752 ; 1471 ; 1980 ; 1897 ; 1544 ; 1914 ; 1923 ; 1944 ; 1375 ; 1987 ; 1993 ; 1742 ; 1975 ; 1479 ; 1977 ; 1934 ; 1939 ; 1950 ; 1992 ; 1983 ; 1474 ; 1643 ; 2010 ; 1814 ; 1942 ; 322 ; 1425 ; 1646 ; 1878 ; 1410 ; 1927 ; 1761 ; 1948 ; 1779 ; 1753 ; 1847 ; 274 ; 1659 ; 1773 ; 1960 ; 1772 ; 1674 ; 1809 ; 1568 ; 1978 ; 1952 ; 1947 ; 1976 ; 1953 ; 1961 ; 1937 ; 1932 ; 1781 ; 2007 ; 1941 ; 1393 ; 1573 ; 1745 ; 169 ; 89 ; 1408 ; 1974 ; 1810 ; 1979 ; 1967 ; 890 ; 1958 ; 1930 ; 1954 ; 1759 ; 720 ; 1936 ; 1576 ; 1407 ; 2004 ; 1964 ; 1462 ; 1875 ; 1943 ; 1938 ; 2006 ; 1739 ; 1378 ; 1922 ; 1924 ; 2003 ; 1792 ; 1985 ; 1729 ; 1966 ; 1355 ; 1940 ; 1928 ; 1357 ; 1955 ; 1896 ; 1115 ; 1836 ; 1971 ; 1329 ; 1807 ; 1997 ; 1359 ; 1801 ; 1933 ; 1965 ; 1981 ; 1711 ; 1905 ; 1625 ; 1968}

let sampleData = seq{1721; 979; 366; 299;675;1456}
let inline orderPair(x,y)=if x>y then (x,y) else (y,x)
let inline orderTriplets(x,y,z)= 
    let (a,b)=orderPair(x,y)
    let (c,d)=orderPair(a,z)
    let (i,j)=orderPair(d,b)
    (c,i,j)
let inline tripleTupleHash(a,b,c)=
    string a.GetHashCode
    + string b.GetHashCode
    + string c.GetHashCode

let dataInTriples = (Seq.allPairs (Seq.allPairs newData newData) newData)  |> Seq.map(fun ((x,y),z)->(x,y,z))

let crossSumIs2020 = dataInTriples |> Seq.filter(fun (x,y,z)->x+y+z=2020)
let expensesThatWork = 
    crossSumIs2020 
    |> Seq.map orderTriplets
    |> Seq.distinctBy tripleTupleHash
[<EntryPoint>]
let main argv =
    printfn "%A" expensesThatWork
    expensesThatWork |> Seq.iter(fun (x,y,z)->printfn "right answer %A" (x*y*z))
    0;