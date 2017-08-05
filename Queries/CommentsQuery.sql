SELECT c.score, 
	COALESCE (c.id) comment_id,
	COALESCE (u.reputation) reputation,
	COALESCE (c.post_id) post_id,
	CASE
              WHEN p.post_type_id = 1 THEN p.tags
              ELSE (SELECT tags FROM posts WHERE id= p.parent_id)
            END,
	 (p.post_type_id) post_type
FROM comments c 
	LEFT JOIN users u 
		ON c.user_id = u.id
	LEFT JOIN posts p 
		ON c.post_id = p.id 

			