create or replace function most_sold_prod( p_start_datetime timestamp, p_end_datetime timestamp)
returns table
( name VARCHAR
)
as
$$

begin
	RETURN QUERY
	select g.name
	from
	(
		select count(d.id) as id_count, d.name
		from invoice a
		left join invoice_details b on a.invoice_id = b.inv_id
		left join prd_list d on b.prd_id = d.id
		where a.date between p_start_datetime and p_end_datetime
		group by d.name
	)g
	order by g.id_count desc
	limit 1;
end;
$$
language plpgsql;


select most_sold_prod('2018-02-18 00:00:00', '2018-08-10 00:00:00')