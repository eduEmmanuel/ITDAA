create or replace function product_invoice_owner( p_invoice_id INT)
returns table
( name VARCHAR, price INT, description VARCHAR, cust_name VARCHAR, surname VARCHAR, middle_name VARCHAR, tel VARCHAR, email VARCHAR
)
as
$$

begin
	RETURN QUERY
	select d.name, d.price, d.description, b.name as cust_name, b.surname, b.middle_name, b.tel, b.email
	from invoice a
	left join customer_list b on a.cust_id =b.id
	left join invoice_details c on a.invoice_id = c.inv_id
	left join prd_list d on c.prd_id = d.id
	where a.invoice_id = p_invoice_id;
end;
$$
language plpgsql;

select product_invoice_owner(7)