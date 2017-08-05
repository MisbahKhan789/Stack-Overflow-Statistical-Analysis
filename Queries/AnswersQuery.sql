SELECT p.comment_count, p.score, p.view_count, q.tags,
	COALESCE (u.reputation) user_erputation,
	COALESCE (u.id) user_id,
	COALESCE (q.id) answer_id,
	CASE 
		WHEN p.id=q.accepted_answer_id THEN 'YES'
		ELSE 'NO'
	END AS ACCEPT

FROM posts p
	LEFT JOIN users u
		ON u.id=p.owner_user_id
	LEFT JOIN posts q 
		ON p.parent_id = q.id
		WHERE p.post_type_id = 2
