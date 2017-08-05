SELECT p.id, p.answer_count, p.comment_count, p.score, p.view_count, p.tags, p.owner_user_id,
	COALESCE (u.reputation) user_reputation
FROM posts p 
	LEFT JOIN users u 
		ON p.owner_user_id = u.id
	WHERE p.post_type_id = 1



